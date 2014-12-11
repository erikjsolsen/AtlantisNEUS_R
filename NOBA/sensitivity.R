### NOBA sensitivity analysis
## Making faceted plots

## Import data sets
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/sensitivity")

#control run
cntr_struct_cap <- readRDS('cntr_mean_struct_cap')
cntr_res_cap <- readRDS('cntr_mean_res_cap')
cntr_nums_cap <- readRDS('cntr_mean_nums_cap')
cntr_all_struct <- readRDS('cntr_mean_struct')
cntr_all_res <- readRDS('cntr_mean_res')
cntr_all_nums <- readRDS('cntr_mean_nums')

#experiment 2-51
struct_w_mult <- readRDS('num_weigth_diff_50_first_struct')
res_w_mult <- readRDS('num_weigth_diff_50_first_res')
num_w_mult <- readRDS('num_weigth_diff_50_first_nums')
#
struct_w_cap <- readRDS('num_weigth_diff_2_50_struct_cap')
res_w_cap <-readRDS('num_weigth_diff_2_50_res_cap')
num_w_cap <- readRDS('num_weigth_diff_2_50_nums_cap')

#experiment 52-61 (combined runs)
struct_w_comb <- readRDS('num_weigth_diff_52_61_struct')
res_w_comb <- readRDS('num_weigth_diff_52_61_res')
num_w_comb <- readRDS('num_weigth_diff_52_61_nums')
#
struct_w_comb_cap <- readRDS('num_weigth_diff_52_61_struct_cap')
res_w_comb_cap <- readRDS('num_weigth_diff_52_61_res_cap')
num_w_comb_cap <- readRDS('num_weigth_diff_52_61_nums_cap')

# All runs
ave_biom_chosen_sp <- as.data.frame(readRDS("ave_biom_chosen_sp"))
names_and_biom_simulations <- as.data.frame(readRDS("names_and_biom_simulations"))
ave_biom_chosen_sp <- as.data.frame(cbind(ave_biom_chosen_sp, names_and_biom_simulations$V6))
colnames(ave_biom_chosen_sp)<-c("cod", "herring", "polcod", "minkewh", "capelin", "run")

#split run column into two
ABC<-as.data.table(ave_biom_chosen_sp)
ll <- unlist(strsplit(as.character(ABC$run), "_", fixed=TRUE))
idx <- seq(1, length(ll), by = 2)
ABC[, `:=`(Adj = ll[idx], Spc = ll[idx+1])]
ABC$Adj[50:59]<-as.character(ABC$run[50:59])
ABC$Spc[50:59] <- c("all")

#to get the "m" or "p" from scenarios
strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
strReverse(c("abc", "Statistics"))
strReverse("Statistics")

ABC$Spc2 <- paste(ABC$Spc, substr(strReverse(as.character(ABC$run)), 5,5), sep="")
ABC$Adj2  <- strReverse(substr(strReverse(as.character(ABC$run)), 6,8))



# give real names
NOBAnames<-c("cod", "herring", "polcod", "minkewh")
rownames(cntr_all_struct)<-NOBAnames
rownames(cntr_all_res)<-NOBAnames
rownames(cntr_all_nums)<-NOBAnames
rownames(struct_w_mult)<-NOBAnames
rownames(res_w_mult)<-NOBAnames
rownames(num_w_mult)<-NOBAnames
rownames(struct_w_comb)<-NOBAnames
rownames(res_w_comb)<-NOBAnames


# Creating faceted plot
ABCmelt <- melt(ABC, id=7:8, measure = 1:5)
ABCplot <-   ggplot(ABCmelt, aes(Spc, value)) +  geom_bar(stat = "identity", aes(fill=Spc)) + theme_bw() + facet_wrap(~ variable + Adj, ncol=9) + theme( axis.text.x = element_text(angle = -60, hjust = 0, colour = "grey20", size=9)) + scale_fill_brewer(palette="Set3")
ABCplot

ggsave("sensitivity panel plot ALL.pdf", scale = 1, dpi = 400)


# Plotting non-combined scenarios
ABCmelt2 <- melt(ABC[1:49], id=6:10, measure = 1:5)
ABCplot2 <-   ggplot(ABCmelt2, aes(Spc, value)) +  geom_bar(stat = "identity", aes(fill=Spc)) + theme_bw() + facet_wrap(~ variable + Adj, ncol=10) + theme( axis.text.x = element_text(angle = -60, hjust = 0, colour = "grey20", size=10)) + scale_fill_brewer(palette="Set3")
ABCplot2

ggsave("sensitivity panel plot single comp.PDF", scale = 1, dpi = 400)

# Plotting non-combined scenarios - more columns, fewer panels
ABCplot3 <-   ggplot(ABCmelt2, aes(Spc2, value)) +  geom_bar(stat = "identity", aes(fill=Spc2)) + theme_bw() + facet_wrap(~ variable + Adj2) + theme( axis.text.x = element_text(angle = -60, hjust = 0, colour = "grey20", size=10)) + scale_fill_brewer(palette="Paired")

ABCplot3 <- ABCplot3 +  theme(strip.text.x = element_text(size=14, face="bold", colour ="white"), strip.background = element_rect(colour="gray40", fill="gray40"))

ggsave("sensitivity panel plot single comp Paired.PDF", scale = 1, dpi = 400)

# combined scenarios
ABCmelt3 <- melt(ABC[50:59], id=6:10, measure = 1:5)
ABCplot4 <-   ggplot(ABCmelt3, aes(run, value)) +  geom_bar(stat = "identity", aes(fill=run)) + theme_bw() + facet_wrap(~ variable) + theme( axis.text.x = element_text(angle = -60, hjust = 0, colour = "grey20", size=10)) + scale_fill_brewer(palette="Set3")
ABCplot4 <- ABCplot4 +  theme(strip.text.x = element_text(size=14, face="bold", colour ="white"), strip.background = element_rect(colour="gray40", fill="gray40"))

ggsave("sensitivity combined.PDF", scale = 1, dpi = 400)


