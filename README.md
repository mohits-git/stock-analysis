# Stock Analysis Dashboard

Stock Analysis Dashboard with R Shiny and Plotly. This dashboard allows users to analyze stock data. Powered with randomForest and caret for stock prediction.

## Get Started

### Prerequisites

- R 4.2.3
- RStudio

### Setup Project

1. Clone the repository
```bash
git clone https://github.com/mohits-git/r-project.git
```

2. Open the project in RStudio

3. Install the required packages

```R
install.packages(c("shiny", "ggplot2", "dplyr", "shinydashboard", "plotly", "zoo", "caret", "randomForest", "conflicted", "rsconnect"))
```

4. Run the app

> Open `run-r-projct.R` in RStudio and click on `Run App` button

### Deploy the app on shinyapps.io

1. Create an account on [shinyapps.io](https://www.shinyapps.io/)

2. Install the `rsconnect` package

```R
install.packages("rsconnect")
```

3. Set .env variables (check `.env.example`) 
```
RS_ACCOUNT_NAME=
RS_TOKEN=
RS_SECRET=
```

4. Deploy the app
> Open `deployment.R` in RStudio and run the full script
