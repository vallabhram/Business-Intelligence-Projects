
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix
import numpy as np

test = pd.read_csv("C:/Vallabh/SELF/Misc/Titanic ML/test.csv")
train = pd.read_csv("C:/Vallabh/SELF/Misc/Titanic ML/train.csv")

train.groupby('Sex')['Survived'].value_counts()
len(train[train['Survived'] == 1]) / len(train)
len(train[(train['Survived'] == 1) & (train['Sex'] == 'male')]) / len(train[train['Sex'] == 'male']) 
len(train[(train['Survived'] == 1) & (train['Sex'] == 'female')]) / len(train[train['Sex'] == 'female']) 

y = train['Survived']
features = ["Pclass", "Sex", "SibSp", "Parch"]

X = pd.get_dummies(train[features])
X_test = pd.get_dummies(test[features])

rfmodel = RandomForestClassifier(n_estimators=100, max_depth=5, random_state=1)
rfmodel.fit(X,y)
predictions = rfmodel.predict(X_test)
test
output = pd.DataFrame({'PassengerId': test['PassengerId'], 'Survived': predictions})
print(output)

train.head()
test.head()


from sklearn.neighbors import KNeighborsClassifier

train_knn = train.select_dtypes(include=['int', 'float'])
train_knn = train_knn.dropna()
test_knn = test.select_dtypes(include=['int', 'float'])
test_knn = test_knn.dropna()
train_knn, X_train, y_train = 
knn_model = KNeighborsClassifier(n_neighbors=1)
knn_model.fit(train_knn.values.reshape(-1,1),test_knn.values.reshape(-1,1))  

print(train_knn.shape)
print(test_knn.shape)




