library(ggplot2);
library(reshape2);
library(svDialogs);

# 01,02,03 -- Female Cyto
# 04,05,06 -- Female Mito
# 07,08,09 -- Male Cyto
# 10,11,12 -- Male Mito

print(dirname(sys.frame(1)$ofile))

dirlist <- list.dirs(recursive=FALSE)
dirlist <- dirlist[dirlist != "./bin1_bin2_bin3_bin4"]
dir1 <- gsub("./", "", dirlist[1])
dir2 <- gsub("./", "", dirlist[2])
dir3 <- gsub("./", "", dirlist[3])

flies = matrix(c(#"Young Male","Young Male"  ,c(-500,8000)   ,c(-500,8000),
                 #"Young Female","Young Female"  ,c(-500,8000)   ,c(-500,8000),
                 #"Old Female","Old Female"  ,c(-500,8000)   ,c(-500,8000),
                 #"Old Male","Old Male"  ,c(-500,8000)   ,c(-500,8000)
                 #"Mated", "Mated", c(0, 13000), c(0,13000),
                 #"Virgin", "Virgin", c(0, 13000), c(0,13000)
                 #"Male", "Male", c(0, 3500), c(0,3500),
                 #"Female", "Female", c(0,3500), c(0,3500)
                 dir1, dir1, c(-2000, 100000), c(-2000,100000),
                 dir2, dir2, c(-2000,100000), c(-2000,100000),
                 dir3, dir3, c(-2000,100000), c(-2000,100000)
                 #"Female","Female"  ,c(-500,8000)   ,c(-500,8000),
                 #"Male Test","Male Test"  ,c(-500,8000)   ,c(-500,8000)
                 #"mitoGFP-Females","mitoGFP-Females"  ,c(-500,9000)   ,c(-500,9000),
                 #"mitoGFP-Males","mitoGFP-Males"  ,c(-500,9000)   ,c(-500,9000)
                 ), ncol=6,byrow=TRUE);
#print(flies)
# "02-07-19-cyto-female-15-10","Female Cyto (15,10)",c(0,500)      ,c(0,9000),
# "02-07-19-mito-male-15-10"  ,"Male Mito (15,10)"  ,c(0,10)       ,c(-25,250),
#"02-07-19-mito-female-15-10","Female Mito (15,10)",c(0,10)       ,c(0,800),
#"02-07-19-mito-male-10-10"  ,"Male Mito (10,10)"  ,c(0,10)       ,c(-50,600),
#"02-07-19-mito-female-10-10","Female Mito (10,10)",c(0,10)       ,c(0,1500),
#"02-07-19-mito-female-06-10","Female Mito (06,10)",c(0,10)       ,c(0,6500),
#"02-07-19-mito-female-15-06","Female Mito (15,06)",c(0,10)       ,c(0,800),
#"02-07-19-mito-female-10-06","Female Mito (10,06)",c(0,10)       ,c(0,1500),
#"02-12-19-cyto-male-15-10"  ,"Male Cyto (15,10)"  ,c(0,3100)     ,c(0,10100),
#"02-12-19-cyto-female-15-10","Female Cyto (15,10)",c(-4000,15000),c(-900,25500),
#"02-12-19-mito-male-15-10"  ,"Male Mito (15,10)"  ,c(0,10)       ,c(0,1500),
#"02-12-19-mito-female-15-10","Female Mito (15,10)",c(0,10)       ,c(0,2400),
#"02-12-19-mito-male-10-10"  ,"Male Mito (10,10)"  ,c(0,10)       ,c(0,1500),
#"02-12-19-mito-female-10-10","Female Mito (10,10)",c(0,10)       ,c(0,2100)),

#VARIABLES TO INITIALIZE
channel = "green";

bins = c("bin1","bin2","bin3","bin4"); # bins to process

print(bins)

{day_0 <- as.numeric(readline(prompt="Enter the start day: "));
 day_last <- as.numeric(readline(prompt="Enter the end day: "))
 tot_days <- as.numeric(readline(prompt="Enter the total no. of days: "))}
scale = TRUE;

path = dirname(sys.frame(1)$ofile);

path = paste0(path,"/");

for(idx in 1:nrow(flies)) {
  datelist <- list.dirs(dirlist[idx], recursive = FALSE) 
  cohort_date = flies[idx,1];
  folder_name = cohort_date;
  cohort = paste0(path, #Always put '/' after the path else it won't work
                  folder_name,"/"); # directory
  title = paste0(cohort_date, "");

  #total test are total vials for a cohort
  #num_replica = 4;
  #total_tests = 4;

  replica_counter = 0;
  file_counter = 0;
  dir_counter = 0;
  total_dirs = 0;
  fly_counter = 0;

  
  #bins = c("bin3","bin4");
  subtitle = paste(bins, collapse = "+");
  lab = flies[idx,2];

  bounds = as.numeric(c(flies[idx,3],flies[idx,4]));
  if("bin1"%in%bins)
    bounds = as.numeric(c(flies[idx,5],flies[idx,6]));

  for(fly in lab) {
    print(fly);                                         #prints category name
    fly_counter = fly_counter + 1;
    df = data.frame(matrix(0, ncol = (1) + 1, nrow = 1));
    y_vars_names = c();
    for(idx in 1:1) {
      y_vars_names = c(y_vars_names, lab[idx]);
    }
    names(df) = c("Day", y_vars_names);

    replica_sum = c();
    regular_sum = c();
    err_dev = c();
    err = data.frame(matrix(0, ncol = 4, nrow = 1));
    save = matrix(0, ncol = 7, nrow = 1);
    save = as.data.frame(save);
    names(save) =  c("day","mean_rep1","mean_rep2","mean_rep3","mean_rep4","avg","sd");
    names(err) = c("Day", "Low", "Top", "Fly");

    for(j in 1:length(datelist)) {      #now goes through each date folder first
      # print(dir)
      vials <- list.dirs(datelist[j], recursive = FALSE)
      total_tests <- length(vials)
      num_replica <- length(vials)
      #
      for(k in 1:length(vials)) {        #for every vial folder in that date
        for(file in list.files(path=vials[k])){
          if(file == "AviFileChunk0.csv") {
            dir <- vials[k]
            file_counter = file_counter + 1;
            dir_counter = dir_counter + 1;
            csv = read.csv(paste0(dir,"/",file));
            
            if(channel == "green")
              csv = csv[-1, c(1, 2, 11:18, 27)];
            if(channel == "blue")
              csv = csv[-1, c(1, 2, 3:10, 27)];
            if(channel == "red")
              csv = csv[-1, c(1, 2, 19:26, 27)];
            
            sum = rep(0, length(csv$frameNum));
            sum_reg = rep(0, length(csv$frameNum));
            
            if("bin1" %in% bins)
              sum = sum + csv[,4];
            if("bin2" %in% bins)
              sum = sum + csv[,6];
            if("bin3" %in% bins)
              sum = sum + csv[,8];
            if("bin4" %in% bins)
              sum = sum + csv[,10];
            
            replica_sum = c(replica_sum, round(sum(sum)/7200, digits = 2));
            regular_sum = c(regular_sum, round(sd(sum), digits = 2));
            
            if (length(replica_sum) == total_tests){
              print(paste0("Mean:",replica_sum))
              print(paste0("SD:",regular_sum))
            }
            
            print ("hello")
            print (replica_sum)
            print ("hello")
            
            if(file_counter == num_replica) {
              file_counter = 0;
              replica_counter = replica_counter + 1;
              save[nrow(save),"day"] = nrow(save);
              save[nrow(save),"mean_rep1"] = round(replica_sum[1], digits = 2);
              save[nrow(save),"mean_rep2"] = round(replica_sum[2], digits = 2);
              save[nrow(save),"mean_rep3"] = round(replica_sum[3], digits = 2);
              save[nrow(save),"mean_rep4"] = round(replica_sum[4], digits = 2);
              
              print (save);
              
              avg_replica = round(sum(replica_sum)/num_replica, digits = 2);
              print(paste0("Total Mean:",avg_replica))
              save[nrow(save),"avg"] = round(avg_replica, digits = 2);
              df[nrow(df),replica_counter+1] = avg_replica;
              sd = sd(replica_sum);
              print(paste0("Total SD:",sd))
              if(is.na(sd)) sd = 0;
              save[nrow(save),"sd"] = round(sd, digits = 2);
              save[nrow(save)+1,] = rep(0, ncol(save));
              err[nrow(err),"Day"] = total_dirs + 1;
              err[nrow(err),"Fly"] = names(df)[replica_counter+1];
              err[nrow(err),"Low"] = avg_replica - sd;
              err[nrow(err),"Top"] = avg_replica + sd;
              err[nrow(err)+1,] = rep(0, ncol(err));
              err_dev = c();
              
              #fileConn<-file("output.txt")
              #writeLines(toString(replica_sum), fileConn)
              #close(fileConn)
              
              replica_sum = c();
              regular_sum = c();
            }
            if(dir_counter == total_tests) {
              df[nrow(df) + 1,] = rep(0, (total_tests/num_replica) + 1);
              dir_counter = 0;
              replica_counter = 0;
              total_dirs = total_dirs + 1;
              print(paste0("Day ",total_dirs," processed..."));
            }
          }
        }
      }
    }
    save = save[-(nrow(save)),];
    print(paste0("Save: ",save))
    print(paste0("Names of save: ",names(save)))
    filename = paste0(cohort,cohort_date,"-data.csv");
    df = df[-(nrow(df)),];
    print (df);
    df["Day"] = seq(1, nrow(df), 1);
    
    err = err[-(nrow(err)),];
    specific_fly = lab[-length(lab)];
    if(!"all" %in% fly) {
      specific_fly = c(fly);
    }
    df = melt(df, id.vars = c("Day"), value.name = "Fluorescence",
              measure.vars = specific_fly);
    col = factor(df$variable);
    ce = err[which(err[,"Fly"]%in%specific_fly),];
    td = total_dirs;

    # change the peak value from here.....
    
    n_0 = save$avg[day_0];
    n_t = save$avg[day_last];                       #make variable by category
    t = day_last - day_0;
    decay_constant = -log(n_t/n_0)/t;
    t_half = (t * (-log(2)))/(log(n_t/n_0));
    t_mean = t_half / log(2);
    
    #print t_half
    subtitle_temp = subtitle
    #subtitle = paste(subtitle,"\n t_half: ",t_half)
    print(paste0("T_Half:",t_half))

    # initially the peak value will be day_0 but you can hardcode it by eyeballing
    # change the peak value from here.....
    half_life_time = seq(from=day_0, to=day_last, length.out=5760);
    half_life_data = n_0 * exp(-(decay_constant * (half_life_time - day_0)));

    data = data.frame(x = half_life_time, y = half_life_data);
    
    print(save)


#     reg_time = seq(from=0, to=3, length.out=4);
#     reg_data = rep(NA, 4);
#     reg_data[1] = save$avg[2];
#     reg_data[2] = save$avg[3];
#     reg_data[3] = save$avg[4];
#     reg_data[4] = save$avg[5];
#     print (reg_data);
# 
#     reg_dat = data.frame(x = reg_time, y = reg_data);
    
    reg_time = seq(from=0, to=day_last-day_0, length.out=day_last-day_0+1);
    reg_data = rep(NA, day_last-day_0+1);
    x1=1;
    for (i in day_0:day_last){
    print (day_0);
    reg_data[x1] = save$avg[i];
    x1=x1+1;
    }
    print (reg_data);
    print (reg_time);
    reg_dat = data.frame(x = reg_time, y = reg_data);
    
    print(paste0("Data used for NLS:", reg_dat))
    f <- function(x, a, b) { a * exp(-b * x); }

    fm0 <- try(nls(log(y) ~ log(a) + (-b * x),
               data = reg_dat,
               start = list(a = n_0, b = t_half)), silent=TRUE);

    print (fm0);

    skip = FALSE;
    if("try-error"%in%attributes(fm0)$class) { skip = TRUE }
    diff = FALSE
    if(!skip) {
      
      model = nls(y ~ f(x, a, b),
                  data = reg_dat,
                  start = coef(fm0));
      
      a <- summary(model)$parameters[1,1];
      b <- summary(model)$parameters[2,1];

      print(paste0("N(0): ",a));
      print(paste0("decay: ",b));
      
      verification_data = a * exp(-(b * (half_life_time - day_0)));
      ver_dat = data.frame(x = half_life_time, y = verification_data);

      Day = df$Day;
      Fluorescence = df$Fluorescence;

      reg_plot_time = half_life_time;
      reg_plot_data = predict(model,
                              data.frame(x = seq(day_0, day_last, length.out = length(reg_plot_time))));

      #print (reg_plot_data);
      
      reg_plot = data.frame(x = reg_plot_time, y = reg_plot_data);
      # print(paste0("Data for plot: ",reg_plot))

      rdf = data.frame(x = seq(day_0, day_last, length.out = (day_last-day_0)+1));
      print (save)
      print (rdf)
      c = cor(predict(model,rdf),save$avg[day_0:day_last]);
      print(paste0("correlation coef............",c))
      
      #Print Correlation coefficient
     # subtitle = paste(subtitle,"\n correlation coefficient: ",c)
      
      d = cor(f(rdf, n_0, b),save$avg[day_0:day_last]);
      print (c);
      #if(abs(c - d) > 0.01) { diff = TRUE; }
      
      rdf5 = data.frame(x = seq(-1, 3, length.out = tot_days));
      print(rdf5)
      print("hello")
      save$reg_nls = (round(predict(model, rdf5), digits = 2));
      print("Next")

      save$reg_nls_cor = round(c, digits = 5);

      save$reg_n0 = round(as.data.frame(f(rdf5, n_0, b))$x, digits = 2);
      save$reg_n0_cor = round(rep(d, nrow(save)), digits = 5);
      save$nls_decay = round(b, digits = 3);
      save$nls_half = round(log(2)/b, digits = 3);
      save$nls_mean = round(1/b, digits = 3);
      subtitle = paste(subtitle,"\n t_half: ",log(2)/b)
      subtitle = paste(subtitle,"\n correlation coefficient: ",c)
    } else {
      save$reg_nls = NA;
      save$reg_nls_cor = NA;
      save$reg_n0 = NA;
      save$reg_n0_cor = NA;
      save$nls_decay = NA;
      save$nls_half = NA;
      save$nls_mean = NA;
    }
    save = save[c(1,2,3,4,6,5,9,7,10,8,11,12,13)];
    write.csv(save, file = filename, row.names = FALSE);

    print(df)
    print(ce)

    ggplot(data = df, mapping = aes(x = Day, y = Fluorescence))+
    scale_colour_manual(values = c("black"))+
    {if(!skip) geom_line(data = ver_dat,
              mapping = aes(x = ver_dat$x, y = ver_dat$y),
              size = 0.03,
              linetype = 1,
              color = "gray20")}+
    {if(diff) geom_line(data = reg_plot,
              mapping = aes(x = reg_plot$x, y = reg_plot$y),
              size = 0.03,
              linetype = 1,
              color = "gray80")}+
    geom_line(size = 0.1,
              linetype = "dotted",
              color = "gray80")+
    geom_point(mapping = aes(color = col),
               size = 0.1,
               show.legend = TRUE)+
    {if(scale) scale_y_continuous(limits=c(-2000,100000))}+
    scale_x_continuous(breaks=seq(0,td,by=1), limits=c(0.75,td+0.25))+
    geom_errorbar(aes(x = ce$Day, ymin = ce$Low, ymax = ce$Top),
                  size = 0.05,
                  width = 0.075,
                  color = "gray5")+
    labs(title = title, subtitle = subtitle, color = "Legend")+
    theme_bw()+
    theme(axis.text=element_text(size=5, face = "bold"),
          axis.title=element_text(size=5, face = "bold"),
          plot.title=element_text(size=7, hjust=0.5),
          plot.subtitle=element_text(size=4, hjust=0.5),
          
          legend.title=element_text(size=5, hjust=0.5),
          legend.text=element_text(size=4),
          panel.background=element_blank(),
          panel.border=element_rect(size=0.1),
          panel.grid=element_blank(),
          axis.ticks=element_line(size=0.1, color="black"),
          axis.line=element_line(size=0.1, color="black"));

    dir_sub = paste(bins, collapse = "_")
    # dir = paste0('/Users/mdemeter/google_drive/gfp/plots/',cohort_date,"/");
    dir = paste0(path,
                 dir_sub,"/");
    if(scale)
      dir = paste0(path,
                   dir_sub,"/");
    if(!dir.exists(dir)) dir.create(dir, recursive = TRUE);
    ggsave(filename = paste0(dir,
                             cohort_date,"-",
                             subtitle_temp,
                             ".tiff"),
           dpi = 1080,
           width = 90,
           height = 50,
           units = "mm");
    total_dirs = 0;
  }
}
