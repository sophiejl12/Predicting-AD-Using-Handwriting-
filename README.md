# Predicting Alzheimer's Disease Using Handwriting
## Introduction
### Alzheimer’s Disease (AD) is a neurodegenerative disease that affects motor and cognitive function. Due to AD’s effect on motor control, handwriting dynamics can be used as an early diagnostic tool. The aim of this project is to identify which handwriting tests are the most indicative of AD, then use these variables to create a Random Forest Model that will predict if a person has AD. The model was trained on the DARWIN dataset, which includes data from 174 participants, some with Alzheimer’s Disease, and some healthy. The participants completed 25 tasks total and 18 measurements were taken from each task. 

## Results 

### <img width="850" alt="Screen Shot 2023-08-01 at 5 43 21 PM" src="https://github.com/sophiejl12/Predicting-AD-Using-Handwriting-/assets/137425759/0bbc454e-ab09-44bb-b9f6-e30b35303afe">

#### To make the model as accurate as possible, it was important to identify which handwriting tests were the most effective in distinguishing AD patients from healthy individuals. Based on the results shown by the graph above, 

### ![Rplot01](https://github.com/sophiejl12/Predicting-AD-Using-Handwriting-/assets/137425759/0fedd36e-1ab6-43fa-8581-069c50d38efa)
### Explain graph

### ![0fba3dae-efeb-4d75-a48d-f826d9ae7624](https://github.com/sophiejl12/Predicting-AD-Using-Handwriting-/assets/137425759/4b880be7-1bd4-4953-91da-df08b4a54aed)
### Explain graph 



The code for this project is located in the Alzheimer's_Project_Code file above.

## Conclusion
### Our final Random Forest model correctly identifies which people have AD based on their handwriting data with an accuracy of 99.28%. With this model, handwriting can be used to identify early signs of AD in a simple, effective, and non-invasive manor. Handwriting data can be collected without a doctor, and requires less money and resources than clinical diagnostic tests. In addition to early diagnosis, handwriting characteristics can be used to continually monitor the effects of treatments. Due to the fact that patients experience varying degrees of AD severity, our model could be improved by identifying which handwriting results correlate with early stage AD, and which results correlate with later stage AD. With this distinction, the model could be trained to identify the severity of each patients’ disease. 

## References
### Fontanella,Francesco. (2022). DARWIN. UCI Machine Learning Repository. https://doi.org/10.24432/C55D0K. 
### Cilia, N. D., De gregorio, G., De stefano, C., Fontanella, F., Marcelli, A., & Parziale, A. (2022). Diagnosing alzheimer's disease from on-line handwriting: A novel dataset and performance benchmarking. Engineering Applications of Artificial Intelligence, 111, 104822. https://doi.org/10.1016/j.engappai.2022.104822 
