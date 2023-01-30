
# ConcurrentProgramming
The following repository contains evidence of different activities in which Principles of Concurrent Programming are implemented.

There are 3 Labs in which each one have different objectives.

## Transpotting
The program controls two trains. The trains move relatively independently of each other, but without colliding. There is a simulator, where one can control the speeds of trains and change rail switches. The simulator gives a signal when a train passes sensors suitably placed on the tracks. There are sensors to make the whole logic work. 
The trains run simultaneously between the two stations. Both trains are able to move at the same time, and are as independent from each other, without crashing. At the stations the trains stop and then wait 2 seconds. The trains run forever between the stations.

## CCHAT
The program is a simple text-based messaging system. CCHAT is very much inspired by IRC, an old but still valid standard designed for group discussions. It leverages Erlang’s processes and message passing features. You can start a server and as many clients as you want, clients can be subscribed to channels in which all clients that are subscribed to them will receive messages from other clients.  

## Amazed
The program implements the parallel version of a sequential search algorithm, using the fork/join model. The goal of the search is finding a goal inside a maze taking advantage of parallelism. There's a character inside a maze and every time there's a possibility to go different ways, a new character is created so they can take different ways to find the goal. They way these characters are created is using the fork/join model. 
Awesome ways to learn.

Commit 1: Example
Commit 2: Example 2
Commit 3: Example 3