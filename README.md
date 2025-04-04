# Gossip Protocol Simulator

The goal of this project is to determine the convergence of gossip and push sum algorithms through a simulator based on actors written in erlang on various network topologies. (full network, line, 2D grid and Imperfect3D).

## Contents of this file

Flow of Program, Prerequisites, Instruction Section, What is working, Largest network dealt with for each type of topology and algorithm

## Flow of Program

* For Gossip and Push-Sum

There are 3 arguments to be passed:

* Input the number of nodes (will be rounded up for random 2d and 3d topologies)
* Input the topology
* Input the algorithm


## Prerequisites

#### Erlang OTP 21(10.0.1)


## Instruction section

#### To run the App

```erlang
(Before running, Goto project directory, where actor.erl is present)
$ cd Project
$ c(actor).
$ :start().
$ Enter <No. of Nodes> <Topology> <Algorithm>
e.g. Enter 10 {line | 2D | Imperfect 3D | full } {gossip | push-sum}
SAMPLE O/P-> Waiting for convergence
.................................................................................................................................................................

Time utilized to converge : xxxx milliseconds
```
Run the app, pass in No. of Nodes, Topology & Algorithm values. The console prints the time taken for the topology to converge for the algorithm and nodes given.


## What is working
Used gossip and push sum protocol for all the given topologies i.e full, line, 2D grid and Imperfect 2D. Started by spawning 10 actors and went up to 10k actors. The convergence is said to be reached when all the actors have heard the rumor for the 10th time. Once the nodes have achieved convergence, they stop transmitting messages to their neighboring nodes. After convergence, we take the printout of the converging point.
These 4 topologies are implemented:

1. Line
2. Full
3. 2D
4. Imperfect 3D


* All of these topologies are working for both Gossip and Push-Sum algorithm.


## Largest Network dealt with for each type of topology and algorithm

The largest network we managed to deal with is 10k nodes for each type of  topology using gossip and push sum algorithms. Decided to limit it to 10k nodes, as beyond that it was difficult to compare between different networks. There are other topology networks for which larger nodes compute in a reasonable amount of time. (full, 2D)

#### For Gossip Algorithm
* Full Topology : 60k nodes
* 2D Topology : 35k nodes
* Imperfect 3D Topology :30k  nodes
* Line Topology : 12k  nodes


#### For Push-Sum Algorithm
* Full Topology :50k nodes
* 2D Topology  : 2k  nodes
* Imperfect 3D Topology: 5k nodes
* Line Topology :1500 nodes
