if (!require(pacman)) install.packages('pacman')
pacman::p_load(dplyr, pastecs,mctest, mvnormalTest)

library(dplyr)
assumptions <- function(x) { #Burada x veri setini ifade etmektedir.
  #ozet verileri olusturmak icin bir data frame olusturuyoruz.
  descr <- as.data.frame(matrix(NA, nrow = 9, ncol = ncol(x)))  
  rownames(descr) <- c("Number_of_Observations", 
                       "Number_of_missing_values", 
                       "min_value", "max_value", 
                       "mode_value", "median_value", "mean_value",
                       "_skewness_", "_kurtosis_") 
  #tan?lay?c? istatistikleri hesapl?yoruz. 
  descriptives <- pastecs::stat.desc(x) 
  Mode = function(x) { 
    ta = table(x)
    tam = max(ta)
    if (all(ta == tam))
      mod = NA
    else
      if(is.numeric(x))
        mod = as.numeric(names(ta)[ta == tam])
    else
      mod = names(ta)[ta == tam]
    return(mod)
  } 
  missing_mean <- function(x) {
    mean(x, na.rm = T)
  }
  #Mod fonksiyonu https://www.r-bloggers.com/computing-the-mode-in-r/ adresinden al?nm??t?r.
  mods <- as.data.frame(apply(as.matrix(x), 2, Mode)) #Mod hesaplayal?m
  descr[1:4, ] <- descriptives[c(1, 3, 4, 5), ]
  descr[5, ] <- mods[1:ncol(x), ]
  descr[6, ] <- descriptives[8, ]
  descr[7, ] <- apply(x, 2, missing_mean)
  descr[8, ] <- moments::skewness(x, na.rm = T)
  descr[9, ] <- moments::kurtosis(x, na.rm = T)-3
  
  #VIF ve TV de?erleri hesaplamak icin modelleri tanimlamak gerekiyor.
  x_new <- x
  x_new$rn <- 1:nrow(x)
  model_for_collinearity <-  lm(
    as.formula(paste(colnames(x_new)[ncol(x_new)], "~",
                     paste(colnames(x_new)[1:(ncol(x_new)-1)], collapse = "+"),
                     sep = ""
    )), data=x_new)
  #VIF ve TV degerlerini hesapliyoruz.
  mc_VIF_TOL <- as.data.frame(mctest::mctest(model_for_collinearity, type = "i")$idiags[,1:2]) 
  mc_CI <- mctest::eigprop(mod = model_for_collinearity)$ci # CI degerini elde ediyoruz.
  
  #coklu dogrusal baglanti istatistiklerinin ozetini elde ediyoruz.
  mc_control <- data.frame(min_VIF = min(mc_VIF_TOL$VIF),
                           max_VIF = max(mc_VIF_TOL$VIF),
                           min_TOL = min(mc_VIF_TOL$TOL),
                           max_TOL = max(mc_VIF_TOL$TOL),           
                           min_CI = min(mc_CI),              
                           max_CI = max(mc_CI) 
                           
  )
  
  #Mahalanobis Uzakligini hesaplayalim
  #Mahalanobis uzakl?ginin hesaplanmasi icin kayip veri bulunmamalidir.  
  distance <- as.matrix(mahalanobis(x, colMeans(x), cov = cov(x)))
  
  Mah_significant <- x %>%
    transmute(row_number = 1:nrow(x), 
              Mahalanobis_distance = distance, 
              Mah_p_value = pchisq(distance, 
                                   df = ncol(x), 
                                   lower.tail = F)) %>%
    filter(Mah_p_value <= 0.001)
  #Mardia'nin basiklik katsayisini hesapliyoruz.
  mardia_kurt <- mvnormalTest::mardia(x)$mv.test[2,]
  
  #mardia carpiklik katsayisi
  mardia_skew <- mvnormalTest::mardia(x)$mv.test[1,] 
  
  #Tanilayici istatistikler, coklu dogrusal baglanti ve Mahalanobis uzakligi hesaplamalari icin bir liste olusturuyoruz.
  return(list(descriptives = round(descr, 2), 
              multicollineartiy =  round(mc_control, 2), 
              Mah_significant =  Mah_significant, 
              n_outlier = nrow(Mah_significant),
              Mardia_Kurtosis = mardia_kurt, 
              Mardia_Skewness = mardia_skew ))
}