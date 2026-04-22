# Spurs Assist Network Analysis

This project studies the San Antonio Spurs’ assist network during the 2024–25 NBA regular season.

The objective was to assess whether player popularity, measured by Instagram followers, is related to the formation of assist ties inside the team. More precisely, the project tests whether more popular players are more likely to receive assist ties from their teammates.

## Project Contents

- Final report
- R Markdown code
- Network datasets
- Graph and community analysis

## Files

- `Project_Report.pdf`: final written report presenting the research question, network construction, QAP test, community analysis, additional results, and conclusions.
- `Project_code.Rmd`: R Markdown file containing the full code used for the analysis.
- `assist_network_code.pdf`: PDF export of the code and outputs.
- `spurs_assist_network_edges_2024_25_regular_season.csv`: dataset containing directed assist ties and assist counts between Spurs players.
- `spurs_instagram_followers_simple.csv`: dataset containing Instagram follower counts used as a player popularity attribute.

## Main Methods

- Directed and weighted network analysis
- Logistic QAP regression
- Louvain community detection
- Descriptive community statistics
- Spearman correlation analysis
- Permutation test

## Main Findings

The binary QAP regression suggests only weak evidence of a popularity mechanism in tie formation. The estimated coefficient is positive but not statistically significant. Community analysis shows that the most popular players are concentrated in one community, although modularity remains low. Additional analysis suggests that popularity is more clearly related to the total number of assists received than to the simple existence of assist ties.

## Tools

- R
- igraph
- sna
- tidygraph
- ggraph
- ggplot2
- dplyr

## Project Type

Group project.
