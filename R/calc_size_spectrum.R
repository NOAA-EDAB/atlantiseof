#slope of log(Abundance) ~ log(Weight_Class)
#need to pull numbers.rds and length_weight.rds

calc_size_spectrum = function(atl.dir, param.dir, fgs.file){
  
  fgs = read.csv(fgs.file)
  
  # biomass.box.invert.file = list.files(atl.dir, 'biomass_box_invert.rds',recursive = T,full.names = T)
  # biomass.box.file = list.files(atl.dir, 'biomass_box.rds',recursive = T,full.names = T)
  # rn.box.file =  list.files(atl.dir, 'RN_box.rds',recursive = T,full.names = T)
  # sn.box.file =  list.files(atl.dir, 'SN_box.rds',recursive = T,full.names = T)
  # len.age.file = list.files(atl.dir, 'length_age.rds',recursive = T, full.names = T)
  
  #get N for inverts
  init.file = list.files(param.dir,'neus_init.nc',recursive = T, full.names = T)
  main.nc.file = list.files(atl.dir, 'neus_output.nc', recursive = T, full.names = T)
  bgm.file = list.files(param.dir, '.bgm',full.names = T)
  
  invert.code = fgs$Code[which(fgs$NumCohorts ==1 & fgs$IsTurnedOn == 1)]
  invert.name = fgs$Name[which(fgs$NumCohorts ==1 & fgs$IsTurnedOn == 1)]
  invert.age.code = fgs$Code[which(fgs$NumCohorts ==2 & fgs$IsTurnedOn == 1)]
  invert.age.name = fgs$Name[which(fgs$NumCohorts ==2 & fgs$IsTurnedOn == 1)]
  invert.code.all = c(paste0('j',invert.age.code), invert.age.code, invert.code)
  
  vert.code  = fgs$Code[which(fgs$NumCohorts > 2 & fgs$IsTurnedOn == 1)]
  vert.name  = fgs$Name[which(fgs$NumCohorts > 2 & fgs$IsTurnedOn == 1)]
  
  #Get N1 and N2 for age structured inverts
  main.nc = ncdf4::nc_open(main.nc.file)
  nc.names = names(nc$var)
  invert.age.nc.names = paste0(invert.age.name,'_N',1:2)
  
  box.area = rbgm::bgmfile(bgm.file)$boxes |> 
    dplyr::select(.bx0, area) |> 
    dplyr::rename(polygon = '.bx0')
  
  box.dz = readRDS(list.files(atl.dir,'nominal_dz.rds',recursive = T, full.names = T)) |> 
    dplyr::mutate(layer = layer) |> 
    dplyr::filter(layer != 5) |> 
    dplyr::rename(dz = 'atoutput')
  
  box.vol = box.area |> 
    dplyr::left_join(box.dz, by = 'polygon') |> 
    dplyr::mutate(volume = area * dz) |> 
    dplyr::select(polygon, layer, volume)
  
  #Make static array of box x layer with volume as variable
  get_invert_age_N = function(name,agecl){
    var = paste0(name,'_N',agecl)
    var.dat = ncdf4::ncvar_get(main.nc, var)
    var.units = ncdf4::ncatt_get(main.nc,var)$units
    var.dim = dim(var.dat)
    nc.times = main.nc$dim$t$vals/86400
    
    if(var.units %in% c('mg N m-3','mg N2 m-3')){
      #remove bottom layer
      var.dat = var.dat[-var.dim[1],,]
      dimnames(var.dat) = list(
        layer =  (nrow(var.dat):1)-1,
        polygon = (1:ncol(var.dat)) -1,
        time = nc.times
      )
    } else if(var.units == 'mg N m-2'){
      var.dat = var.dat[var.dim[1],,]
      dimnames(var.dat) = list(
        layer =  var.dim[1],
        polygon = (1:ncol(var.dat)) -1,
        time = nc.times
      )
    }
    var.df = as.data.frame.table(var.dat) |> 
      dplyr::mutate(layer = as.integer(as.character(layer)),
                    polygon = as.integer(as.character(polygon)),
                    time = as.integer(as.character(time))) |> 
    dplyr::rename(atoutput = 'Freq') |> 
    dplyr::mutate(species=name,agecl = agecl)
    
    return(var.df)
  }
  
  #combinations of invert.age.name and agecl 1,2
  invert.age.combs = expand.grid(name = invert.age.name, agecl = 1:2)
  invert.age.ls = list()
  for(i in 1:nrow(invert.age.combs)){
    print(invert.age.combs$name[i])
    invert.age.ls[[i]] = get_invert_age_N(invert.age.combs$name[i], invert.age.combs$agecl[i])
  }
  invert.age.n =  dplyr::bind_rows(invert.age.ls) |> 
    dplyr::filter(atoutput>0)
    
  
  invert.n = atlantistools::load_nc(nc = main.nc.file,
                                    fgs = fgs.file,
                                    bps = bps.names,
                                    select_groups = invert.name,
                                    select_variable = 'N',
                                    prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                    bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  )
  
  
  
  #Get total biomass from density
  invert.n.all = invert.n |> 
    dplyr::bind_rows(invert.age.n) |> 
    dplyr::rename(LongName = 'species') |> 
    dplyr::left_join(dplyr::select(fgs, Code, Name, LongName)) |> 
    dplyr::mutate(is.epi = Name %in% bps.names,
                  layer = ifelse(layer == 4, NA, layer)) |> 
    dplyr::left_join(box.area) |> 
    dplyr::left_join(box.vol) |> 
    dplyr::mutate(mgN = ifelse(is.epi, 
                               atoutput * area, 
                              atoutput * volume))
  
  vert.sn = atlantistools::load_nc(nc = main.nc.file,
                                    fgs = fgs.file,
                                    bps = bps.names,
                                    select_groups = vert.name,
                                    select_variable = 'StructN',
                                    prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                    bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  ) |> 
    dplyr::rename(StructN = 'atoutput',
                  LongName = 'species')
  
  vert.rn = atlantistools::load_nc(nc = main.nc.file,
                                   fgs = fgs.file,
                                   bps = bps.names,
                                   select_groups = vert.name,
                                   select_variable = 'ResN',
                                   prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                   bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  )|> 
    dplyr::rename(ResN = 'atoutput',
                  LongName = 'species')
  
  vert.num = atlantistools::load_nc(nc = main.nc.file,
                                   fgs = fgs.file,
                                   bps = bps.names,
                                   select_groups = vert.name,
                                   select_variable = 'Nums',
                                   prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                   bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  )|> 
    dplyr::rename(num = 'atoutput',
                  LongName = 'species')
  
  #Get size for inverts
  get_global_param = function(param){
    param.line = grep(param,bio.lines, value =T)
    if(length(param.line) == 0){
      return(NA)
    }else{
      param.split = strsplit(param.line, ' |\t')[[1]]
      param.split = param.split[param.split!='']
      param.val = as.numeric(param.split[2])
      return(param.val)
    }
  }
  bio.file = list.files(param.dir, 'at_biology.prm',full.names = T)
  bio.lines = readLines(bio.file)
  li.a.invert = get_global_param('li_a_invert')
  li.b.invert = get_global_param('li_b_invert')
  xrs = get_global_param('X_RS')
  xcn = get_global_param('X_CN')
  wetdry = get_global_param('wetdry')

  invert.sn = data.frame(Code = invert.code.all,
                         sn =sapply(invert.code.all,function(x){
                                    sn.val = get_global_param(paste0('^',x,'_sn'))
                                    return(sn.val)
                                    })
  )
  
  #Get vertebrate li_a and li_b
  
  vert.lw = data.frame(Code = vert.code,
                       li.a =sapply(vert.code,function(x){
                                        sn.val = get_global_param(paste0('^li_a_',x))
                                        return(sn.val)
                                      }),
                       li.b=sapply(vert.code,function(x){
                         sn.val = get_global_param(paste0('^li_b_',x))
                         return(sn.val)
                       })
  )
  
  #convert N to length
  len.num.invert = invert.n.all |> 
    dplyr::left_join(invert.sn, by = 'Code') |> 
    dplyr::mutate(wgt.ind.mgN = (1+ xrs)*sn,
                  wgt.ind.g = wgt.ind.mgN * wetdry * xcn * 1E-3,
                  num = mgN/wgt.ind.g,
                  len.cm = (wgt.ind.g/li.a.invert)^(1/li.b.invert)) |> 
    dplyr::select(Code, agecl, polygon, layer, time,len.cm, wgt.ind.g, num) |> 
    dplyr::mutate(layer = ifelse(is.na(layer), 4, layer),
                  time = floor(time/365))
    
    
  #Get length for verts
  len.num.vert = vert.sn |> 
    dplyr::left_join(vert.rn) |> 
    dplyr::left_join(vert.num) |> 
    dplyr::left_join(dplyr::select(fgs, Code, Name, LongName)) |> 
    dplyr::left_join(vert.lw) |> 
    dplyr::mutate(wgt.ind.g = (ResN + StructN) * wetdry * xcn * 1E-3,
                  len.cm = (wgt.ind.g/li.a)^(1/li.b)) |> 
    dplyr::select(Code, agecl, polygon, layer, time,len.cm, wgt.ind.g, num) |> 
    dplyr::filter(!is.na(num))
  
  
  
  #combine length data
  len.num.all = len.num.invert |> 
    dplyr::bind_rows(len.num.vert) |> 
    dplyr::left_join(dplyr::select(fgs, Code, Name, LongName)) |> 
    dplyr::mutate(biomass = wgt.ind.g * num)
  
  
  
  
  
}