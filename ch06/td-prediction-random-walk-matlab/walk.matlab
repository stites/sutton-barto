
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BEGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reinforcement Learning.
% 7/11/01 JV Stone, Psychology, Sheffield University. j.v.stone@shef.ac.uk
%
% This matlab code implements an example given in Sutton's 1988 paper on
% temporal difference learning and
% in his recent book. The example consists of a Markov chain of 5 nodes plus
% two absorbing states (A and G):
%
%	A B C D E F G
%
% with corresponding stateid numbers
%
%	0 1 2 3 4 5 6
% 
% The 'reward' for states A and G are 0 and 1, resp.
% The transition probabilities P of moving to the right (ie p(1)) are:
%
%	P = [] 1/2 1/2 1/2 1/2 1/2 []
%
% The probability p of ending in state G from each state is:
%
%	T = [] 1/6 2/6 3/6 4/6 5/6 []
%
% Thus p(G|B)=1/6.
% The problem consists of learning the transition probabilities T.
%
% T is learned over a series of random walks (nwalks).
% T is estimated as a weight vector w.
% The problem of temporal credit assignment arises here because
% the reward is only given at the end (state A or G) of each walk,
% so that the final reward associated with moving into each state must be
% estimated long after that state has been left.

% Typical output for parameter values in this file:
%
% Initial w: w =
%     0.6507    0.8875    0.5692    0.8798    0.6063
% Final w: w =
%     0.1544    0.3235    0.4915    0.6373    0.8246
% Required solution: T =
%     0.1667    0.3333    0.5000    0.6667    0.8333
% Rms error = 0.035

clear all;
nstates     = 5; % B to F
T           = [1:nstates]/(nstates+1);
P           = ones(nstates,1)/2;
states      = diag(ones(nstates,1)); % each col vec is a state
w           = rand(1,nstates);
dw          = zeros(size(w));    % change in w.
elast       = zeros(size(w));    % error at t-1 (sum in eq 4).
lambda      = 0.3;     % exponential decay - 0.3 better than 1
alpha       = 0.05;    % learning rate
nwalks      = 1000;    % number of random walks
intervalw   = 10;    % number of walks between weight update.

% Anneal value of learning rate.
a=ones(nwalks,1)*0.999;b=[1:nwalks]';
alphas=(a.^b)/10; figure(2); plot(alphas); title('Learning rate');
% Dont anneal
alphas = ones(nwalks,1)*alpha;

fprintf('Initial w: '); w

for i=1:nwalks
    dw      = dw*0;
    walking = 1;
    alpha   = alphas(i);

    % Init params for walk.
    stateid = 3;
    xt      = states(:,stateid);
    Pt      = w*xt;
    elast   = 0;

    while walking
        % Move left or right
        % next_stateid=get_next_stateid(P,stateid);
        pright = P(stateid);
        if rand < pright % move right
            next_stateid = stateid+1;
        else % move left
            next_stateid = stateid-1;
        end;

        % Find Pnext
        if next_stateid==0
            Pnext = 0;
            walking = 0;
        elseif next_stateid==nstates+1
            Pnext = 1;
            walking = 0;
        else
            xnext = states(:,next_stateid);
            Pnext = w*xnext;
        end;

        % Find change in w.
        et = lambda*elast + xt';   % error at time t
        dP = Pnext-Pt;             % Change in predicted final reward made at time t+1. 
        dw = dw + dP*et;

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % DEBUG
        db=0;
        if db
        [stateid next_stateid Pnext Pt dP ]
        dP*elast
        pr;
        end;
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Update variables.
        Pt      = Pnext;
        elast   = et;
        xt      = xnext;
        stateid = next_stateid;

    end; % walking

    % Update w
    if rem(i,intervalw)==0
        w = w + alpha*dw;
    end;

end; % nwalks
fprintf('Final w: '); w
fprintf('Required solution: '); T
fprintf('Rms error = %.3f\n', norm(w-T));
f(1);
xx=1:nstates;plot(xx,T,'k',xx,w,'r');
title('RL: Optimal (black), estimated (red)');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
