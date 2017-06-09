from matplotlib import pyplot as plt
import numpy as np
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split, StratifiedKFold

def loadFromBackup(filename):
    with open(filename, 'r') as f:
        desc = f.readline()
        rows, cols, nBinaryMatrices, dontChangeColorsInt = list(map(int,f.readline().split(" ")))
        realMappedRows = list(map(int,f.readline().split(" ")))
        realMappedCols = list(map(int,f.readline().split(" ")))

        labelX = np.array([i.replace("|%|", " ") for i in f.readline().rstrip().split(" ")])
        labelY = np.array([i.replace("|%|", " ") for i in f.readline().rstrip().split(" ")])

        dataset = np.array([[0 for j in range(cols)] for i in range(rows)])

        for n in range(nBinaryMatrices):
            for r in range(rows):
                for c in list(map(int,[i for i in f.readline().rstrip().split(" ") if i!=""])):
                    dataset[r][c] = n if nBinaryMatrices > 1 else 1
            f.readline() # skip one line

        dataset = dataset.take(realMappedRows, axis = 0).take(realMappedCols, axis = 1)
        labelX = labelX.take(realMappedCols)
        labelY = labelY.take(realMappedRows)

    return dataset, labelX, labelY

def nearest_neighbors(labels, train, indice, k = 10):
    up = indice-1
    down = indice+1
    nLabels = len(set(labels))
    neighbors = [0 for i in range(nLabels)]
    count = 0

    while count < k and (0 <= up or down < len(labels)):
        if 0 <= up:
            if up in train:
                neighbors[labels[up]] += 1
                count += 1
            up -= 1

        if down < len(labels) and count < k:
            if down in train:
                neighbors[labels[down]] += 1
                count += 1
            down += 1

    return np.argmax(neighbors)

def getPredictedLabels(labels, train, test):
    predicted_labels = []
    for i in test:
        predicted_labels.append(nearest_neighbors(labels, train, i))
    return predicted_labels

def getAccuracy(dataset, labels, K = 10):
    accs = []
    distinct_labels = list(set(labels))
    new_labels = np.array([distinct_labels.index(i) for i in labels])

    for train_indices, test_indices in StratifiedKFold(n_splits=K, shuffle=True).split(dataset, new_labels):
        predicted_labels = getPredictedLabels(new_labels, train_indices, test_indices)
        acc = accuracy_score(new_labels.take(test_indices), predicted_labels)
        accs.append(acc)

    return np.array(accs).mean()

import os
for fn in os.listdir('accuracies'):
    if not fn.endswith(".DS_Store"):
        directoryName = 'accuracies/'+fn
        for f in os.listdir(directoryName):
             if f.endswith(".backup"):
                 dataset, labelX, labelY = loadFromBackup(directoryName+'/'+f)
                 acc = getAccuracy(dataset, labelY)
                 print('rows %s %.2f' % (f, acc * 100) )

                 if f.startswith("foot") or f.startswith("doc"):
                    acc = getAccuracy(dataset, labelX)
                    print('cols %s %.2f' % (f, acc * 100) )
