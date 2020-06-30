## Requirements

Fill in the ./data/ directory with appropriate CSVs

```
docker build -t step-browser .

docker run -p 3838:3838 -it step-browser
```