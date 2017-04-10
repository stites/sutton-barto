/*
This is an example program for reinforcement learning with linear 
function approximation.  The code follows the psuedo-code for linear, 
gradient-descent Sarsa(lambda) given in Figure 8.8 of the book 
"Reinforcement Learning: An Introduction", by Sutton and Barto.

This version is kept simple, at the cost of efficiency.  
Eligibility traces are implemented naively.  Features sets are arrays.

Before running the program you need to load the tile-coding 
software, available at http://envy.cs.umass.edu/~rich/tiles.C and tiles.h
(see http://envy.cs.umass.edu/~rich/tiles.html for documentation).

The code below is in two main parts: 1) General RL code, and
2) Mountain Car code.

Written by Rich Sutton 12/17/00
 */

#include <iostream>
#include "tiles.h"
#include "stdlib.h"

#define N 3000                         // number of parameters to theta, memory size
#define M 3                            // number of actions
#define NUM_TILINGS 10                 // number of tilings in tile coding

// Global RL variables:
float Q[M];                            // the action values
float theta[N];                        // modifyable parameter vector, aka memory, weights
float e[N];                            // eligibility traces
int F[M][NUM_TILINGS];                 // sets of features, one for each action
    
// Standard RL parameters:
#define epsilon 0.0                    // probability of random action
#define alpha 0.5                      // step size parameter
#define lambda 0.9                     // trace-decay parameters
#define gamma 1                        // discount-rate parameters

// Profiles:
int episode(int max_steps);            // do one episode, return length
void load_Q();                         // compute action values for current theta, F
void load_Q(int a);                    // compute one action value for current theta, F
int argmax(float Q[M]);                // compute argmax action from Q
bool with_probability(float p);        // helper - true with given probability
void load_F();                         // compute feature sets for current state
void mcar_init();                      // initialize car state
void mcar_step(int a);                 // update car state for given action
bool mcar_goal_p ();                   // is car at goal?

int main()
// The main program just does a bunch or runs, each consisting of some episodes.
// It prints out the length (number of steps) of each episode.
   {for (int run=0; run<10; run++)
       {cout << "Beginning run #" << run << endl;
        for (int i=0; i<N; i++) theta[i]= 0.0;                     // clear memory at start of each run
        for (int episode_num=0; episode_num<100; episode_num++) 
            cout << episode(10000) << endl;}
    return 0;}
            
int episode(int max_steps)
// Runs one episode of at most max_steps, returning episode length; see Figure 8.8 of RLAI book
   {mcar_init();                                                   // initialize car's state
    for (int i=0; i<N; i++) e[i] = 0.0;                            // clear all traces
    load_F();                                                      // compute features
    load_Q();                                                     // compute action values
    int action = argmax(Q);                                        // pick argmax action
    if (with_probability(epsilon)) action = rand() % M;            // ...or maybe pick action at random
    int step = 0;                                                  // now do a bunch of steps
    do {step++;
        for (int i=0; i<N; i++) e[i] *= gamma*lambda;              // let traces fall
        for (int a=0; a<M; a++)                                    // optionally clear other traces
            if (a != action)
                for (int j=0; j<NUM_TILINGS; j++) e[F[a][j]] = 0.0;
        for (int j=0; j<NUM_TILINGS; j++) e[F[action][j]] =1.0;    // replace traces
        mcar_step(action);                                         // actually take action
        float reward = -1;
        float delta = reward - Q[action];
        load_F();                                                  // compute features new state
        load_Q();                                                 // compute new state values
        action = argmax(Q);
        if (with_probability(epsilon)) action = rand() % M;
        if (!mcar_goal_p()) delta += gamma * Q[action];
        float temp = (alpha/NUM_TILINGS)*delta;
        for (int i=0; i<N; i++) theta[i] += temp * e[i];           // update theta (learn)
		load_Q(action);}
    while (!mcar_goal_p() && step<max_steps);                      // repeat until goal or time limit
    return step;}                                                  // return episode length

void load_Q() 
// Compute all the action values from current F and theta
   {for (int a=0; a<M; a++) 
        {Q[a] = 0;
         for (int j=0; j<NUM_TILINGS; j++) Q[a] += theta[F[a][j]];}}

void load_Q(int a) 
// Compute an action value from current F and theta
   {Q[a] = 0;
    for (int j=0; j<NUM_TILINGS; j++) Q[a] += theta[F[a][j]];}

int argmax(float Q[M])
// Returns index (action) of largest entry in Q array, breaking ties randomly
   {int best_action = 0;
    float best_value = Q[0];
    int num_ties = 1;                    // actually the number of ties plus 1
    for (int a=1; a<M; a++) 
       {float value = Q[a];
        if (value >= best_value) 
            if (value > best_value)
                 {best_value = value;
                  best_action = a;}
            else {num_ties++;
                  if (0 == rand() % num_ties)
                     {best_value = value;
                      best_action = a;}}};
    return best_action;}
    
bool with_probability(float p)
// Returns TRUE with probability p
   {return p > ((float)rand()) / RAND_MAX;}

    
///////////////  Mountain Car code begins here  ///////////////

// Mountain Car Global variables:
float mcar_position, mcar_velocity;

#define mcar_min_position -1.2
#define mcar_max_position 0.6
#define mcar_max_velocity 0.07            // the negative of this is also the minimum velocity
#define mcar_goal_position 0.5

#define POS_WIDTH (1.7 / 8)               // the tile width for position
#define VEL_WIDTH (0.14 / 8)              // the tile width for velocity

void load_F()
// Compute feature sets for current car state
   {float state_vars[2];
    state_vars[0] = mcar_position / POS_WIDTH;
    state_vars[1] = mcar_velocity / VEL_WIDTH;
    for (int a=0; a<M; a++)
        GetTiles(&F[a][0],NUM_TILINGS,state_vars,2,N,a);}

void mcar_init()
// Initialize state of Car
   {mcar_position = -0.5;
    mcar_velocity = 0.0;}

void mcar_step(int a)
// Take action a, update state of car
   {mcar_velocity += (a-1)*0.001 + cos(3*mcar_position)*(-0.0025);
    if (mcar_velocity > mcar_max_velocity) mcar_velocity = mcar_max_velocity;
    if (mcar_velocity < -mcar_max_velocity) mcar_velocity = -mcar_max_velocity;
    mcar_position += mcar_velocity;
    if (mcar_position > mcar_max_position) mcar_position = mcar_max_position;
    if (mcar_position < mcar_min_position) mcar_position = mcar_min_position;
    if (mcar_position==mcar_min_position && mcar_velocity<0) mcar_velocity = 0;}

bool mcar_goal_p ()
// Is Car within goal region?
   {return mcar_position >= mcar_goal_position;}



