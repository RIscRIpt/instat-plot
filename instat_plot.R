packages <- c(
    "dplyr",
    "ggplot2"
)

packages_new <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages_new)) {
    install.packages(packages_new)
}
lapply(packages, require, character.only=TRUE)

f <- file("stdin")
open(f)
instructions <- c()
counts <- c()
stats <- c()
while (length(line <- readLines(f, n=1)) > 0) {
    stats <- c(stats, line)
}
stats <- as.data.frame(matrix(unlist(strsplit(stats, ":")), ncol=2, byrow=TRUE))
colnames(stats) <- c("Instruction", "Count")
stats$Count <- as.numeric(stats$Count)

p <- stats %>%
    top_n(50, Count) %>%
    filter(Instruction != "int3" & Instruction != "nop") %>%
    ggplot(mapping=aes(x=reorder(Instruction, -Count), y=Count)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
        labs(x="Instruction", y="Count")

ggsave("stats.pdf", p, width=8, height=4)
