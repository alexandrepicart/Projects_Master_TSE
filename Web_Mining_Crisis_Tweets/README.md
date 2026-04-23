# Web Mining – Crisis Tweets

This project was completed as part of a Web Mining course at the Toulouse School of Economics.

It focuses on mining knowledge from social media data during crisis events, using tweets collected from real-world emergencies.

## Project Contents

- Final report
- Jupyter notebook
- Semantic analysis of tweets
- Question-answering system
- Tweet priority prediction

## Files

- `Web_Mining_Project_Report.pdf`: final written report presenting the dataset, preprocessing pipeline, semantic clustering, QA system, and priority prediction results.
- `Project_Notebook.ipynb`: notebook containing the code used for preprocessing, embedding-based analysis, retrieval, and classification.

## Main Methods

- Text cleaning and tokenization
- GloVe word embeddings
- Sentence-Transformer tweet embeddings
- K-Means clustering
- PCA and t-SNE visualization
- Two-stage retriever–re-ranker
- Logistic Regression
- Random Forest
- XGBoost
- SMOTE
- Hyperparameter optimization

## Data

The project uses a crisis-tweet dataset from the NIST TREC Incident Stream Initiative.

The working dataset contains 55,986 tweets across 6 event types: bombing, earthquake, flood, shooting, typhoon, and wildfire.

## Main Results

The question-answering pipeline achieved perfect Recall@10 on the test queries, with stronger performance on specific factual queries than on broader ones.

For tweet priority prediction, Random Forest and XGBoost outperformed Logistic Regression, and the best optimized XGBoost model reached an F1 macro score of 0.561.

## Project Type

Group project completed at TSE.
