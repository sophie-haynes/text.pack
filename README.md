# text.pack
Text mining helper functions for R. 
## Installation
```
install.packages("devtools")
library(devtools)
devtools::install_github('sophie-haynes/text.pack')
library(text.pack)
```
## Functions
### `punctStrip()`
* Strips punctuation from a corpus object. Requires the corpus as a parameter.

### `grammar()`
* Applies stemming to a corpus object. Requires the corpus as parameter. 

  ***N.B.** This function requires you to have [`TreeTagger`][1] installed on your system locally.*

----------------------------------------------------

***License:**  MIT*

***Author:** Sophie Haynes*

[1]: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
