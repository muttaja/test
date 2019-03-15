import pandas as pd
import numpy as np
# data=pd.read_csv('A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/SMI/d601.csv')
# #print(data.head())
# print(data.columns)
#
# df1 = data[['B02_kevad1', 'B02_kevad2', 'B02_sygis',
#        'B03_kevad1', 'B03_kevad2', 'B03_sygis', 'B04_kevad1', 'B04_kevad2',
#        'B04_sygis', 'B05_kevad1', 'B05_kevad2', 'B05_sygis', 'B06_kevad1',
#        'B06_kevad2', 'B06_sygis', 'B07_kevad1', 'B07_kevad2', 'B07_sygis',
#        'B08_kevad1', 'B08_kevad2', 'B08_sygis', 'B11_kevad1', 'B11_kevad2',
#        'B11_sygis', 'B12_kevad1', 'B12_kevad2', 'B12_sygis', 'B02_vahe_kevad',
#        'B03_vahe_kevad', 'B04_vahe_kevad', 'B05_vahe_kevad', 'B06_vahe_kevad',
#        'B07_vahe_kevad', 'B08_vahe_kevad', 'B11_vahe_kevad', 'B12_vahe_kevad',
#        'muld', 'ARV_VMA', 'ARV_VKU', 'ARV_VKS', 'ARV_VXX']]
#
# #koos mullaga
# df2 = data[['B02_kevad1', 'B02_kevad2', 'B02_sygis',
#        'B03_kevad1', 'B03_kevad2', 'B03_sygis', 'B04_kevad1', 'B04_kevad2',
#        'B04_sygis', 'B05_kevad1', 'B05_kevad2', 'B05_sygis', 'B06_kevad1',
#        'B06_kevad2', 'B06_sygis', 'B07_kevad1', 'B07_kevad2', 'B07_sygis',
#        'B08_kevad1', 'B08_kevad2', 'B08_sygis', 'B11_kevad1', 'B11_kevad2',
#        'B11_sygis', 'B12_kevad1', 'B12_kevad2', 'B12_sygis', 'B02_vahe_kevad',
#        'B03_vahe_kevad', 'B04_vahe_kevad', 'B05_vahe_kevad', 'B06_vahe_kevad',
#        'B07_vahe_kevad', 'B08_vahe_kevad', 'B11_vahe_kevad', 'B12_vahe_kevad',
#        '21', '42', '43', '44', '45', '48', '51', '61', '64', '77', '999']]
#
# target = ['ARV_VMA', 'ARV_VKU', 'ARV_VKS', 'ARV_VXX']
# #NB hetkel ilma mullata
# xnames = ['B02_kevad1', 'B02_kevad2', 'B02_sygis',
#        'B03_kevad1', 'B03_kevad2', 'B03_sygis', 'B04_kevad1', 'B04_kevad2',
#        'B04_sygis', 'B05_kevad1', 'B05_kevad2', 'B05_sygis', 'B06_kevad1',
#        'B06_kevad2', 'B06_sygis', 'B07_kevad1', 'B07_kevad2', 'B07_sygis',
#        'B08_kevad1', 'B08_kevad2', 'B08_sygis', 'B11_kevad1', 'B11_kevad2',
#        'B11_sygis', 'B12_kevad1', 'B12_kevad2', 'B12_sygis', 'B02_vahe_kevad',
#        'B03_vahe_kevad', 'B04_vahe_kevad', 'B05_vahe_kevad', 'B06_vahe_kevad',
#        'B07_vahe_kevad', 'B08_vahe_kevad', 'B11_vahe_kevad', 'B12_vahe_kevad']
# print(target)
# print(df1.columns)
#
# dftarget = df1[target]
# dfX = df1[xnames]
# print(dftarget.head())
#
# from sklearn import tree
# from sklearn.model_selection import cross_val_score
# clf = tree.DecisionTreeRegressor()
#
# #print(df1.muld)
#
# clf = tree.DecisionTreeRegressor()
#
# clf = clf.fit(dfX, dftarget)
# print("nohh?")
#
#
#
# print(dftarget.head(10))



###for tsükkel?
#in range 601 kui kõik
# d = []
# for i in range(601):
#     dfX0 = df2.drop(df2.index[i])
#     dftarget0 = dftarget.drop(dftarget.index[i])
#
#     clf = tree.DecisionTreeRegressor()
#     clf = clf.fit(dfX0, dftarget0)
#     dfX1 = df2.iloc[i, :]
#     dfX1 = dfX1.values.reshape(1,-1)
#     #print(dfX1)
#     clfp = clf.predict(dfX1)
#     #tst.append(i)
#     print(clfp)
#     d.append(clfp)
#
# print("kas töötab?")

# dd2 = pd.DataFrame(np.array(d).reshape(601,4), columns=list("abce"))
# dd2.to_csv("cart_muld.csv", sep=",", index=False)

#max_depth = 2

# d = []
# for i in range(601):
#     dfX0 = df2.drop(df2.index[i])
#     dftarget0 = dftarget.drop(dftarget.index[i])
#
#     clf = tree.DecisionTreeRegressor(max_depth=40, min_samples_leaf= 2)
#     clf = clf.fit(dfX0, dftarget0)
#     dfX1 = df2.iloc[i, :]
#     dfX1 = dfX1.values.reshape(1,-1)
#     #print(dfX1)
#     clfp = clf.predict(dfX1)
#     #tst.append(i)
#     print(clfp)
#     d.append(clfp)
#
# dd_max40 = pd.DataFrame(np.array(d).reshape(601,4), columns=list("abce"))
# dd_max40.to_csv("cart_max40_msl2.csv", sep=",", index=False)
#
# d = []
# for i in range(0,601):
#     dfX0 = df2.drop(df2.index[i])
#     dftarget0 = dftarget.drop(dftarget.index[i])
#
#     clf = tree.DecisionTreeRegressor(max_depth=40,min_samples_leaf= 4)
#     clf = clf.fit(dfX0, dftarget0)
#     dfX1 = df2.iloc[i, :]
#     dfX1 = dfX1.values.reshape(1,-1)
#     clfp = clf.predict(dfX1)
#     print(clfp)
#     d.append(clfp)
#
# dd_max40 = pd.DataFrame(np.array(d).reshape(601,4), columns=list("abce"))
# dd_max40.to_csv("cart_max40_msl4_600rida.csv", sep=",", index=False)

# for j in range(9,13):
#     for md in range(10,42,2):
#         d = []
#         for i in range(601):
#             dfX0 = df2.drop(df2.index[i])
#             dftarget0 = dftarget.drop(dftarget.index[i])
#             clf = tree.DecisionTreeRegressor(max_depth=md,min_samples_leaf=j)
#             clf = clf.fit(dfX0, dftarget0)
#             dfX1 = df2.iloc[i, :]
#             dfX1 = dfX1.values.reshape(1,-1)
#             clfp = clf.predict(dfX1)
#             print(clfp)
#             d.append(clfp)
#         dd_max = pd.DataFrame(np.array(d).reshape(601,4), columns=list("abce"))
#         dd_max.to_csv("cart_max"+str(j)+str(md)+".csv", sep=",", index=False)


