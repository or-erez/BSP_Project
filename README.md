
<br />
<p align="center">

  <h3 align="center">Parallel processing of large graphs from social networks via BSP</h3>
  <h5 align="center">Or Erez, Jonathan Karin</h5>

  <p align="center">
    Out final project on 'Concurrent and Distributed Programming' course, Ben Gurion University of the Negev.
    <br />
  </p>
</p>



<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the Project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [Contact](#contact)



<!-- ABOUT THE PROJECT -->
## About The Project
Huge amount of interesting information can be gained by analyzing large-scale data that is derived from social networks.

Social networks are often modeled as graphs, usually known as social graph.

Social graphs of popular social networks (for example: graph that represents all Apple employees on LinkedIn) can contains large amount of vertexes and edges .

Our project offers a distributed soloution for analyzing big graphs, Our project support tree diffrent algorithms:

* Breadth First Search
* Shortest Path Between Two Vertices (Bellman Ford)
* Minimum Spanning Tree

### Built With
* [Erlang](https://www.erlang.org/)


<!-- GETTING STARTED -->
## Getting Started

Open folder and terminal for the 'master' node, open a folder and terminal for each one of the submasters.
in each terimnal write:
```sh
git clone https://github.com/or-erez/BSP_Project
cd BSP_Project
```

<h3>Submaster: </h3>
Do the next stages for every submaster.
if you use erlang short name, start erlang:
for submaster x :

```sh
erl -sname submasterx
```
if you use erlang long name:
```sh
erl -name submasterx@IP -setcookie bsp
```
compile 'subMaster' and 'worker':
```sh
c(subMaster).
c(worker).
```
start the submaster:
```sh
{_,S}=subMaster:start_link().
```
<h3>Master: </h3>

if you use erlang short name, start erlang:
```sh
erl -sname master
```

if you use erlang long name:
```sh
erl -name master@IP -setcookie bsp
```
compile 'master' and 'gui':
```sh
c(master).
c(gui).
```
start the program:
```sh
gui:start(node()).
```


<!-- USAGE EXAMPLES -->
## Usage
<br />

* Choose the file of the graph - txt file that contains all the edges of the graph sorted lexicographically. <b> make sure that this file is exist in all the main folders of all the submasters</b>. 
* Choose algorithm.
* Insert the short/long names of all the submasters separated by comma .
* Chhose start/ende vertex.
* Click 'start'
* Wait for answer.
<br />
<br />
![GitHub Logo](/1.jpg)
Format: ![example](https://github.com/or-erez/BSP_Project)


<!-- CONTACT -->
## Contact

Or Erez - orere [at ] post.bgu.ac.il

Jonathan Karin - karinjo [at ] post.bgu.ac.il

