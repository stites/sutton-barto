/* This code was written by Abhinav Garg, abhinav@cs.umass.edu, August, 1998 */

# include <stdio.h>
# include <math.h>
# include <stdlib.h>

# define n       10
# define h        0.5
# define p        6
# define alpha    0.01
# define beta     0.01
# define epsilon  10
# define iterations  2000000

# define max(a,b) (((a)>(b)) ? (a) : (b));

double q[n+1][4][2] = {0.0}; /* 0 - reject, 1 - accept */
 
/* To generate a priority request */ 
int req_priority(void)
{
  int req = 0;
  /*  req = rand() % 4;
  return req; */ /* CHECK OUT LATER */
  req = rand() %100;
  if (req<40)
   return 3;
  else if (req >= 40 && req <60)
   return 2;
  else if (req >=60 && req <80)
   return 1;
  else if (req >=80 && req <100)
  return 0;
}
 
/* Find out whether server is to be freed or not */ 
int free_server(void)
{
  int free = 0;
  free  = (rand() % 100);
  if ( free < p )
     return 1;      /* free the server */
  return 0;      /* server is still busy */
}

int  choose_action(int r, int c)
     /* r,c are the state's  subscripts and q is the array */
{
  int number;
  int row, column ;

  if (r==0)
    return 0;

  number = (rand() % 100) ;

  
  if (number > epsilon)
     { 
       if ( q[r][c][0] > q[r][c][1] )
	  return 0;
	 /* {printf("\n1"); fflush(stdout); return 0;} */
       else if  ( q[r][c][0] < q[r][c][1] )
	  return 1;
         /* {printf("\n2"); ffluqsh(stdout); return 1;} */
       else if  ( q[r][c][0] == q[r][c][1] )
          return (rand() % 2 );
         /* {printf("\n3");fflush(stdout);  return (rand() % 2 );} */
     }
  else
          return (rand() % 2 );
         /* {printf("\n4");fflush(stdout); return (rand() % 2 );} */
 }

  int states_optimal_action(int r ,int c)
 {
  if (r == 0) 
    return 0;
  if ( q[r][c][0] > q[r][c][1] )
    return 0;
  else 
    return 1;
 }
  
  int request_reward(int a)
  { 
    if (a==0)
      return 1;
    else if (a==1)
      return 2;
    else if (a==2)
      return 4;
    else if (a==3)
      return 8;
  }
 

 main (int argc, char ** argv)
{
  double rho = 0.0;
  int reward;
  int i, j, k;
  int server_free = n ; /* Current no. of free servers */
  int server_state[n];
  int action , step ;
  int cur_state[2], next_state[2];
  double next_optimal_q_value, best_q;

  /* Making all servers initially free */
  for (i = 0; i< n; i++)
    server_state[i] = 1;
  
   /* Initialization */
   srand(123456);
 
   cur_state[0] = n;
   cur_state[1] = req_priority();
   server_free = n;

   for(step = 0; step <= iterations ; step++)
     {   
       /* Choose an action */ 
       action = choose_action(cur_state[0], cur_state[1]);
            
       /* Find reward */
       reward = request_reward(cur_state[1]);

       if (action==0)
	 reward = 0;
        
       /* Whether to free a busy server or not */
       for(i = 0; i<n ; i++)
	 { 
	   if (server_state[i] == 0)
	     server_state[i] = free_server(); /* free the busy server */
	 }
       
       /*Find out how many servers are free */
       server_free = 0;
       for(i = 0; i<n ; i++)
	 {
	   if (server_state[i] == 1)
	     server_free++;
	 }

       if (action == 1)
	 server_free--;

       if (server_free<0 || server_free>=n+1)
	 {
	   printf("error!\n");
	   getchar();
	 }

       
       for (i=0; i<server_free; i++)
	 server_state[i] = 1;
       
       for (i=server_free; i<n; i++)
	 server_state[i] = 0;
       
       next_state[0] = server_free;
       next_state[1] = req_priority();
       
       /*printf("%d %d\n",  next_state[0],   next_state[1]);
	 getchar();*/
       
       if (next_state[0]==0)
	 next_optimal_q_value = q[next_state[0]][next_state[1]][0];
       else
	 next_optimal_q_value = max(q[next_state[0]][next_state[1]][0], 
				    q[next_state[0]][next_state[1]][1]);

       q[cur_state[0]][cur_state[1]][action]
	 += alpha*(  reward - rho
		     + next_optimal_q_value
		     - q[cur_state[0]][cur_state[1]][action] );


       if (cur_state[0]==0)
	 best_q = q[cur_state[0]][cur_state[1]][0];
       else
	 best_q = max(q[cur_state[0]][cur_state[1]][0], q[cur_state[0]][cur_state[1]][1]);
       


       if (fabs(q[cur_state[0]][cur_state[1]][action]-best_q)<=0.00000001)
	 rho += beta*(reward  - rho
		      + next_optimal_q_value
		      - best_q);    

       cur_state[0] = next_state[0];
       cur_state[1] = next_state[1];   
      
       if (step % 500000 == 0)
	 { 
	   printf("\n Step: %d ", step);
	   for( i = 0; i <= n; i++)
	     for( j = 0; j <= 3 ; j++)
	       for( k = 0; k <= 1 ; k++)
		 printf("\n q[%d][%d][%d] = %lf",i,j,k,q[i][j][k]);
	   getchar();
	 }
 
       
     }
   printf("rho=%lf\n", rho);
}
