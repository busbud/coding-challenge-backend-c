Demo at https://tranquil-chamber-64444.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

#Limitations
Query parameters order is fixed to ?q=&lat=&lon=

#Improvments to be considered
Number of suggestions returned could be limited
The load of the data can be improved in many ways, but as it is done at startup, I did not dig into the possibilities.

#Tests:
Added some
'npm test' command crashes on my machine, but I can run tests from VS Code

#Denormalization
The TSV file is parsed into 2 JSON files for index. This adds an additional step at each source file change. 
I prefer this solution to the direct parsing of TSV because providing a raw file to a service may block the starting of this service if the raw input has some errors

The denormalization is done by running app-denormalize.js

#Streams
As I don't know streams yet, I'm not sure of the overhead and of the use of map/mapSync. So I sticked to safest uses.

#Score
Score is poorly implemented, by a lack of idea on how to tackle this part.

#Update
JSONStream for output
Cleaning