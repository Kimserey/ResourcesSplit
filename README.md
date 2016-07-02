# Resources Split

An application to split resources based on Level and Continent.

## 1. The partitioning strategy

Each level has a score attributed 1-2-3. A resource is placed where:

 1. The group with the __lowest total Level score__ is chosen
 2. When multiple groups have the same Level score, __the first group with the least resource on the Continent of the resource__ target is chosen

## 2. How to use

### 2.1. Select the level and continent which matches the resources

![resources](https://raw.githubusercontent.com/Kimserey/ResourcesSplit/master/resources.png)

### 2.2. View the result partitioning based on Level and Continent

![result](https://raw.githubusercontent.com/Kimserey/ResourcesSplit/master/results.png)
