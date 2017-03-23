# jingoh.org - Jingoh's background database system

## Abstract concepts
Jingoh.org provides jingoh's background database system.
Provides abstract container type named ORG and its operators.

## Terms
### ORG
Stands in ORGanization.
It manages all REQUIREMENTs of SUBJECTs about your system.
You can refer it as test-suite which usual test framework has.

### REQUIREMENT
What the operator must/should/may behaves.
You can refer it as test which usual test framework has.

### SUBJECT
Target of REQUIREMENT.

## Management hierarchy
```
+-----+
| ORG | ; e.g. system or package
+-----+
   |
+----------------------------------------+
|+----------+    			 | ; called "specifications"
|| SUBJECTs | ; e.g. functions or macros |
|+----------+				 |
|   |					 |
|+--------------+			 |
|| REQUIREMENTS | ; i.e. behaviors       |
|+--------------+			 |
+----------------------------------------+
```
