# Baseball and Markov Chains

## Baseball's Structure

Baseball has a relatively clean, distinct structure. Each plate appearance can be viewed as a separate matchup or event, and there are a fixed number of possible baserunner/out combinations, or states, for each plate appearance. Specifically, each of the three bases can be either occupied or unoccupied, and there can be zero, one, or two outs, leaving 2<sup>3</sup> * 3 = 24 possible states. 

While different plate appearances are distinct and exhibit a strong degree of independence, the results do rely heavily on which of the 24 aforementioned states the game is in before the start of plate appearance. For example, a double with nobody on base has a much different effect on the game than one with the bases loaded. As such, we want a way to combine the independence of individual plate appearances with the dependence of these events within innings. 

## Markov Chains

A Markov chain is a random probability model that exhibits one-step dependence. Put another way, the probability of transitioning from state $i$ to state $j$ is always the same, no matter what happened before, and only depends on state $i$. This is called the Markov property, and it simplifies probability calculations: we don't have to account for the entire past, only the most recent state. 

In a baseball sense, this means that the probability of an event happening is just dependent on the starting state, or baserunner/out combination. If we say $p_{i,j}$ is the probability of going from state $i$ to state $j$, we can construct a 25 by 25 matrix containing all these probabilities, which is called the transition matrix (25, not 24, because we also need to be able to transition to an inning-ending three-out state). 