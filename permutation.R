#permutation test
r = cor(RestaurantTips$Bill,RestaurantTips$PctTip)

#upper-tail test
pvalue1 = sum(replicate(1000, cor(RestaurantTips$Bill,sample(RestaurantTips$PctTip)) > r))/1000
pvalue1


#two-tailed test
pvalue2 = sum(replicate(1000, abs(cor(RestaurantTips$Bill,sample(RestaurantTips$PctTip))) > abs(r)))/1000
pvalue2


noOutliers = RestaurantTips[(RestaurantTips$PctTip <= 30),]


#permutation test without outliers

#upper-tail test
pvalue3 = sum(replicate(1000, cor(noOutliers$Bill,sample(noOutliers$PctTip)) > r))/1000
pvalue3

pvalue4 = sum(replicate(1000, abs(cor(noOutliers$Bill,sample(noOutliers$PctTip))) > abs(r)))/1000
pvalue4
