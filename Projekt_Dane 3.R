list.of.packages <- c("car", "naniar", "Hmisc", "ggplot2", "dplyr",
                      "knitr", "magrittr", "kableExtra", "dunn.test",
                      "FSA", "ggpubr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]


if(length(new.packages))
  install.packages(new.packages)

library("naniar")
library("Hmisc")
library("ggplot2")
library("dplyr")
#install.packages("knitr")
library("knitr")
library(magrittr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("car")
library(car)
#install.packages("dunn.test")
#install.packages("FSA")
library(dunn.test)
library(FSA)
library(ggpubr)
#POCZATEK PUNKT1


args <- commandArgs(trailingOnly = TRUE)
data <- read.csv2(args[1])


data_num <- select_if(data, is.numeric)

data[data == "NA"] <- NA

for(i in 1:ncol(data)) {
  if(sum(is.na(data[ , i])) > 0){
    
    data[which(is.na(data[ , i])), i] <- mean(data[ , i], na.rm = TRUE)
    
  }
}

#podzial danych w zaleznosci od grupy
data_split <- split(data, data$grupa)
ilosc_grup <- length(names(data_split))

#rozdzial danych podzielonych w zaleznosci od grupy na tylko numeryczne!

czy_numeryczne <- list()
podzielone_numeryczne <- list()

for(i in names(data_split)){
  czy_numeryczne[[i]] <- sapply(data_split[[i]], is.numeric)
  podzielone_numeryczne[[i]] <- data_split[[i]][ ,czy_numeryczne[[i]]]
  
}


for(grupa in podzielone_numeryczne){
  for(col in names(grupa)){
    median_val <- median(grupa[[col]])
    q1 <- quantile(grupa[[col]], 0.25)
    q3 <- quantile(grupa[[col]], 0.75)
    iqr <- q3 - q1
    data[[col]][data[[col]] < (q1 - 1.5 * iqr) | data[[col]] > (q3 + 1.5 * iqr)] <- median_val
  }
}


kolumny_num <-sum(sapply(data, is.numeric))
png("box.png", width=300*length(podzielone_numeryczne), height=300*kolumny_num)
par(mfrow=c(kolumny_num, length(podzielone_numeryczne)))

for(j in 2:ncol(data_split[[i]])){
  for(i in 1:length(data_split)){
    if(is.numeric(data_split[[i]][[j]])){
      bx <- boxplot(data_split[[i]][[j]], main=c(unique(data_split[[i]][,1]), 
                                                      colnames(data_split[[i]])[j]))
     
    }
  }
}

podsumowanie <- list()
for(i in names(data)){
  if(is.numeric(data[[i]])){
    podsumowanie_i <- data %>%
      group_by(grupa) %>%
      summarise(
        mean = format(round(mean(.data[[i]], na.rm = TRUE), 2), nsmall = 2),
        sd = format(round(sd(.data[[i]], na.rm = TRUE), 2), nsmall = 2),
        median = format(round(median(.data[[i]], na.rm = TRUE), 2), nsmall = 2),
        min = format(round(min(.data[[i]], na.rm = TRUE), 2), nsmall = 2),
        max = format(round(max(.data[[i]], na.rm = TRUE), 2), nsmall = 2)
        
      )
    podsumowanie[[i]] <- podsumowanie_i
  }
  
}

print(podsumowanie)

podsumowanie_o <- capture.output(print(podsumowanie))

write(podsumowanie_o, file="raport.txt", append=TRUE)



pvalue_shapiro <- list()
for(i in names(data)){
  if(is.numeric(data[[i]])){
    test_i <- data %>%
      group_by(grupa) %>%
      summarise(
        #statistics = shapiro.test(.data[[i]])$statistics,
        p.value = shapiro.test(.data[[i]])$p.value
      )
    pvalue_shapiro[[i]] <- test_i
  }
}

defaultW<-getOption("warn")
options(warn=-1)
pvalue_levene <- list()

for(i in names(data)){
  if(is.numeric(data[[i]])){
    test_i <- data %>%
      group_by(grupa) %>%
      summarise(
        p.value = car::leveneTest(as.formula(paste0(i, " ~ grupa")), data = .)$"Pr(>F)"[1]
      ) %>%
      ungroup()
    #print(test_i)
    pvalue_levene[[i]] <- test_i
  }
}

options(warn=defaultW)

for (i in names(pvalue_shapiro)) {
  
  tmp <- pvalue_shapiro[[i]]
  if (min(tmp[,"p.value"]) > 0.05) {
    print(paste0("Wszystkie wartości z kolumny: ", i, " należą do rozkładu normalnego"))
    write(paste0("Wszystkie wartości z kolumny: ", i, " należą do rozkładu normalnego"), file="raport.txt",append=TRUE)
  }
}

for (i in names(pvalue_levene)) {
  tmp <- pvalue_levene[[i]]
  tmp1 <- 1
  if (min(tmp[,"p.value"]) > 0.05) {
    print(paste0("Wartości z kolumny: ", i, " są homogeniczne"))
    write(paste0("Wartości z kolumny: ", i, " są homogeniczne"), file="raport.txt", append=TRUE)
  }
}

#tu bede trzymal wartosci homologiczne i wartosci ktore pasuja do rozkladu normalnego
rozklad_normalny <- data.frame(matrix(NA, nrow = length(pvalue_shapiro), ncol = 1))
row.names(rozklad_normalny) <- names(pvalue_shapiro)

for (i in names(pvalue_shapiro)) {
  tmp <- pvalue_shapiro[[i]]
  count <- sum(tmp[, "p.value"] > 0.05)
  rozklad_normalny[i, ] <- count == nrow(tmp)
}
names(rozklad_normalny) <- "rozklad"

homogenicznosc <- data.frame(matrix(NA, nrow = length(pvalue_levene), ncol = 1))
row.names(homogenicznosc) <- names(pvalue_levene)

for (i in names(pvalue_levene)) {
  tmp <- pvalue_levene[[i]]
  count <- sum(tmp[, "p.value"] > 0.05)
  homogenicznosc[i, ] <- count == nrow(tmp)
}
names(homogenicznosc) <- "homogenicznosc"

porownywanie_grup <- cbind(rozklad_normalny, homogenicznosc)

for(i in names(data)[sapply(data, is.numeric)]){
  p <- ggplot(data, aes_string(x = i)) +
    geom_density(aes(fill = grupa), alpha = 0.5) +
    facet_wrap(~grupa, scales = "free") +
    labs(x = i, y = "gestosc") +
    scale_fill_manual(values = c("#99cc00", "#660099", "#0047b3"))
  
  ggsave(paste0("./", i, ".png"), p, width = 7, height = 5)
}



#testy grupowe

defaultW<-getOption("warn")
options(warn=-1)
pvalue_levene <- list()

iter <- length(names(pvalue_shapiro))
for(i in 1:iter){
  
  if(ilosc_grup == 2){
    
    if(porownywanie_grup[i,1] == TRUE && porownywanie_grup[i,2] == TRUE){
      cat("===test t-Studenta (dla gr. niezależnych)=== \n")
      write(paste0("===test t-Studenta (dla gr. niezależnych)=== \n"), file="raport.txt", append=TRUE)
      
      wynik <- t.test(data_num[, i] ~ grupa, data=data, var.equal = TRUE)
      pstudent <- wynik$p.value
      
      if(pstudent < 0.05){
        
        cat(pstudent, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
            names(pvalue_shapiro)[i], ", pomiędzy grupami\n")
        write(paste0(pstudent, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
              names(pvalue_shapiro)[i], ", pomiędzy grupami\n"), file="raport.txt", append=TRUE)
        
      }else{
        cat(pstudent, "> 0.05 Brak istotnych roznic dla badania:", names(pvalue_shapiro)[i],"\n")
        write(paste0(pstudent, "> 0.05 Brak istotnych roznic dla badania:", names(pvalue_shapiro)[i],"\n"), file="raport.txt", append=TRUE)
      }
      
      
      
    }else if(porownywanie_grup[i,1] == TRUE && porownywanie_grup[i,2] == FALSE){
      cat("===test Welcha===\n")
      write(paste0("===test Welcha===\n"), file="raport.txt", append=TRUE)
      
      wynik <- t.test(data_num[, i] ~ grupa, data=data, var.equal = FALSE)
      pwelch <- wynik$p.value
      
      if(pwelch < 0.05){
        
        cat(pwelch, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
            names(pvalue_shapiro)[i], ", pomiędzy grupami\n")
        write(paste0(pwelch, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
              names(pvalue_shapiro)[i], ", pomiędzy grupami\n"), file="raport.txt", append=TRUE)
        
      }else{
        cat(pwelch, "> 0.05 Brak istotnych roznic dla badania:", names(pvalue_shapiro)[i],"\n")
        write(paste0(pwelch, "> 0.05 Brak istotnych roznic dla badania:"), names(pvalue_shapiro)[i],"\n", file="raport.txt", append=TRUE)
      }
      
      
    }else if(porownywanie_grup[i,1] == FALSE){
      cat("===test Wilcoxona (Manna-Whitneya)===\n")
      write(paste0("===test Wilcoxona (Manna-Whitneya)===\n"), file="raport.txt", append=TRUE)
      
      wynik <- wilcox.test(data_num[, i] ~ grupa, data=data)
      pwilcox <- wynik$p.value
      
      if(pwilcox < 0.05){
        
        cat(pwilcox, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
            names(pvalue_shapiro)[i], ", pomiędzy grupami\n")
        write(paste0(pwilcox, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
              names(pvalue_shapiro)[i], ", pomiędzy grupami\n"), file="raport.txt", append=TRUE)
        
      }else{
        cat(pwilcox, "> 0.05 Brak istotnych roznic dla badania:", names(pvalue_shapiro)[i],"\n")
        write(paste0(pwilcox, "> 0.05 Brak istotnych roznic dla badania:", names(pvalue_shapiro)[i],"\n"), file="raport.txt", append=TRUE)
      }
      
    }
    
  }else if(ilosc_grup > 2){
    if(porownywanie_grup[i,1] == TRUE && porownywanie_grup[i,2] == TRUE){
      cat("===test ANOVA (post hoc Tukeya)===\n")
      write(paste0("===test ANOVA (post hoc Tukeya)===\n"), file="raport.txt", append=TRUE)
      
      wynik <- aov(data_num[, i] ~ grupa, data=data)
      pANOVA <- summary(wynik)[[1]][["Pr(>F)"]][1]
      
      if(pANOVA < 0.05){
        posthoc <- TukeyHSD(wynik)
        ptukey <- posthoc$grupa[, "p adj"]
        for(j in length(ptukey)){
          if(ptukey[j] < 0.05){
            cat(pANOVA, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
                names(pvalue_shapiro)[i], ", pomiędzy grupami: ", rownames(posthoc$grupa)[j], "\n")
            write(paste0(pANOVA, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
                  names(pvalue_shapiro)[i], ", pomiędzy grupami: ", rownames(posthoc$grupa)[j], "\n"),file="raport.txt", append=TRUE)
          }
        }
        
        
      } else {
        cat(pANOVA, "> 0.05 Brak istotnych roznic dla badania:", names(pvalue_shapiro)[i],"\n")
        write(paste0(pANOVA, "> 0.05 Brak istotnych roznic dla badania:", names(pvalue_shapiro)[i],"\n"), file="raport.txt",append=TRUE )
      }
      
      
    }else if(porownywanie_grup[i,1] == TRUE && porownywanie_grup[i,2] == FALSE){
      cat("===test Kruskala-Wallisa (post hoc Dunna)===\n")
      write(paste0("===test Kruskala-Wallisa (post hoc Dunna)===\n"), file="raport.txt", append=TRUE)
      
      wynik <- kruskal.test(data_num[, i] ~ grupa, data=data)
      pKruskal <- wynik$p.value
      
      if(pKruskal < 0.05){
        
        dunn_wyniki <- dunn.test(data_num[, i], data$grupa, kw=TRUE)
        for(j in 1:length(dunn_wyniki$comparison)){
          if(dunn_wyniki$P.adjusted[j] < 0.05){
            cat(pKruskal, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
                names(pvalue_shapiro)[i], ", pomiędzy grupami: ", dunn_wyniki$comparison[j], "\n", )
            write(paste0(pKruskal, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
                  names(pvalue_shapiro)[i], ", pomiędzy grupami: ", dunn_wyniki$comparison[j], "\n"), file="raport.txt", append=TRUE)
          }
        }
        
      }else if(pKruskal > 0.05){
        cat(pKruskal, "> 0.05 Brak istotnych roznic pomiedzy grupami dla badania:", names(pvalue_shapiro)[i], "\n")
        write(paste0(pKruskal, "> 0.05 Brak istotnych roznic pomiedzy grupami dla badania:", names(pvalue_shapiro)[i], "\n"), file="raport.txt", append=TRUE)
      }
      
      
    }else if(porownywanie_grup[i,1] == FALSE){
      cat("===test Kruskala-Wallisa (post hoc Dunna)===\n")
      write(paste0("===test Kruskala-Wallisa (post hoc Dunna)===\n"), file="raport.txt", append=TRUE)
      
      wynik <- kruskal.test(data_num[, i] ~ grupa, data=data)
      pKruskal <- wynik$p.value
      
      
      if(pKruskal < 0.05){
        capture.output(dunn_wyniki <- dunn.test(data_num[, i], data$grupa, kw=TRUE))
        for(j in 1:length(dunn_wyniki$comparison)){
          if(dunn_wyniki$P.adjusted[j] < 0.05){
            cat(pKruskal, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
                names(pvalue_shapiro)[i], ", pomiędzy grupami: ", dunn_wyniki$comparison[j], "\n")
            write(paste0(pKruskal, "< 0.05 - są różnice pomiędzy grupami dla badania:", 
                  names(pvalue_shapiro)[i], ", pomiędzy grupami: ", dunn_wyniki$comparison[j], "\n"), file="raport.txt", append=TRUE)
          }
        }
        
      }else if(pKruskal > 0.05){
        cat(pKruskal, "> 0.05 Brak istotnych roznic pomiedzy grupami dla badania:", names(pvalue_shapiro)[i], "\n")
        write(paste0(pKruskal, "> 0.05 Brak istotnych roznic pomiedzy grupami dla badania:", names(pvalue_shapiro)[i], "\n"), file="raport.txt", append=TRUE)
      }
      
    }
  }
}
options(warn=defaultW)
#TESTY KORELACJI
for(i in names(data_split)){
  print(i)
  write(paste0(i, "\n"), file="raport.txt", append=TRUE)
  
  nie_num <- names(data_split[[i]])[sapply(data_split[[i]], is.numeric)]
  
  for(j in 1:(length(nie_num) - 1)){
    for(k in (j+1):length(nie_num)){
      testk <- cor.test(data_split[[i]][[nie_num[j]]], data_split[[i]][[nie_num[k]]], method = "pearson")
      
      if(0.5 <= testk$p.value && testk$p.value < 0.7){
        cat("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest silną korelacją dodatnią. \n")
        write(paste0("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest silną korelacją dodatnią. \n") ,file = "raport.txt", append=TRUE)
      }else if(0.7 <= testk$p.value && testk$p.value < 1){
        cat("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest bardzo silną korelacją dodatnią.\n")
        write(paste0("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest bardzo silną korelacją dodatnią.\n"), file = "raport.txt", append=TRUE)
      }else if(-0.7 < testk$p.value && testk$p.value <= -0.5){
        cat("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest  silną korelacją ujemną.\n")
        write(paste0("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest  silną korelacją ujemną.\n"), file = "raport.txt", append=TRUE)
      }else if(-1 < testk$p.value && testk$p.value <= -0.7){
        cat("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest bardzo silną korelacją ujemną.\n")
        write(paste0("Korelacja pomiedzy: ",nie_num[j],"i", nie_num[k]," jest bardzo silną korelacją ujemną.\n"), file = "raport.txt", append=TRUE)
        
      }
    }
  }
}






