library(ggplot2)

rand_vect_cont <- function(N, M, sd = 1) {
  # generates vector of random elements length N that sums to M
  
  vec <- runif(N, M/N, sd)
  vec / sum(vec) * M
}


# specify how many samples to plot and which "target ancestry" ancestry to sort by
num_samples <- 1000
target_ancestry <- 'EUR'

# build vector for all ancestry labels
ancestry_codes <- c('AFR', 'EUR', 'WAS')
ancestries <- rep(ancestry_codes, num_samples)

# build vector for probabilites of each sample belonging to each ancestry
set.seed(42)
probs <- c()
for (i in 1:num_samples) {
  probs <- c(probs,rand_vect_cont(length(ancestry_codes),1))
}


# build vector for sample ids
ids <- rep(1:num_samples, each=length(ancestry_codes))

# construct dataframe from ancestry, prob, and id vectors
df <- data.frame('ancestry'=ancestries,
                      'prob'=probs,
                      'id'=ids)


# nedd an additional vector filled with probs of just the target ancestry for sorting
df$target_prob <- ifelse(df$ancestry == target_ancestry, df$prob, NA)

# sort dataframe by target ancestry, then make ids factor to preserve order when plotting
df_ordered <- df[order(df$target_prob),]
df_ordered$id <- factor(df_ordered$id, levels=unique(df_ordered$id))


# set order of barplot stacks, placing target ancestry in top stack
stack_order <- c(target_ancestry, ancestry_codes[!ancestry_codes %in% c(target_ancestry)])


# make the plot
p <- ggplot(df_ordered, aes(x=id, y=prob, fill=factor(ancestry, levels=stack_order))) + geom_col()
p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p <- p + labs(x='Participant', y='Ancestry Fraction', fill='Ancestry')
p

