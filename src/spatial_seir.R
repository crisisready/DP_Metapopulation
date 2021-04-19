# adapted from https://github.com/SenPei-CU/COVID-19
# https://science.sciencemag.org/content/sci/suppl/2020/03/13/science.abb3221.DC1/abb3221_Li_SM.pdf
# https://science.sciencemag.org/content/368/6490/489.full
# http://mathesaurus.sourceforge.net/octave-r.html


spatial_seir <- function(x, M, pop, ts, pop0) {
  num_loc = length(pop)
  dt = 1 #timestep = 1 day
  tmstep = 1 #integrate forward for one day

  Sidx = seq(from =1, to = 5*num_loc, by = 5)
  Eidx = seq(from =2, to = 5*num_loc, by = 5)
  Isidx = seq(from =3, to = 5*num_loc, by = 5)
  Iaidx = seq(from =4, to = 5*num_loc, by = 5)
  obsidx = seq(from =5, to = 5*num_loc, by = 5)
  betaidx = 5*num_loc + 1
  muidx = 5*num_loc + 2
  thetaidx = 5*num_loc + 3
  Zidx = 5*num_loc + 4
  alphaidx = 5*num_loc + 5
  Didx = 5*num_loc + 6

  S = matrix(0, num_loc, tmstep + 1)
  E = matrix(0, num_loc, tmstep + 1)
  Is = matrix(0, num_loc, tmstep + 1)
  Ia = matrix(0, num_loc, tmstep + 1)
  Incidence = matrix(0, num_loc, tmstep + 1)
  obs = matrix(0, num_loc, tmstep + 1)

  #intialize
  S[,1] = x[Sidx]
  E[,1] = x[Eidx]
  Is[,1] = x[Isidx]
  Ia[,1] = x[Iaidx]
  beta = x[betaidx]   # transmission rate due to symptomatic individuals
  mu = x[muidx]       # transmission rate due to asymp. individuals is reduced by a factor mu
  theta = x[thetaidx] # mult. factor, which is >1 to reflect underreporting of human movement
  Z = x[Zidx]         # average latency period
  alpha = x[alphaidx] # fraction of documented (or symptomatic) infections
  D = x[Didx]         # average duration of infection

  # We assume that individuals in the Is group do not move between cities,
  # though these individuals can move between cities during the latency period.

  # Mij is the daily number of people traveling from city j to city i

  # start integration
  tcnt = 0
  for (t in seq(from = (ts+dt), to = (ts+tmstep), by = dt)) {


    tcnt=tcnt+1
    dt1=dt

    #first step
    ESenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (S[,tcnt]/(pop-Is[,tcnt])))
    ESleft = pmin(dt1 * (rep(1, num_loc)*theta) * (S[,tcnt]/(pop-Is[,tcnt])) * colSums(M[[ts]]), dt1*S[,tcnt])

    EEenter = dt1* (rep(1,num_loc)*theta) * (M[[ts]] %*% (E[,tcnt]/(pop-Is[,tcnt])))
    EEleft = pmin(dt1 * (rep(1, num_loc)*theta) * (E[,tcnt]/(pop-Is[,tcnt])) * colSums(M[[ts]]), dt1*E[,tcnt])

    EIaenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (Ia[,tcnt]/(pop-Is[,tcnt])))
    EIaleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Ia[,tcnt]/(pop-Is[,tcnt])) * colSums(M[[ts]]), dt1*Ia[,tcnt])

    Eexps = dt1*(rep(1,num_loc)*beta) * S[,tcnt] * Is[,tcnt]/pop
    Eexpa = dt1*(rep(1,num_loc)*mu) * (rep(1,num_loc)*beta) * S[,tcnt] * Ia[,tcnt]/pop
    Einfs = dt1*(rep(1, num_loc)*alpha) * E[,tcnt]/(rep(1,num_loc)*Z)
    Einfa = dt1*(rep(1, num_loc)*(1-alpha)) * E[,tcnt]/(rep(1,num_loc)*Z)
    Erecs = dt1*Is[,tcnt]/(rep(1,num_loc)*D)
    Ereca = dt1*Ia[,tcnt]/(rep(1,num_loc)*D)

    ESenter = pmax(ESenter,0); ESleft = pmax(ESleft,0)
    EEenter = pmax(EEenter,0); EEleft = pmax(EEleft,0);
    EIaenter = pmax(EIaenter,0); EIaleft= pmax(EIaleft,0);
    Eexps = pmax(Eexps,0); Eexpa = pmax(Eexpa,0);
    Einfs = pmax(Einfs,0); Einfa = pmax(Einfa,0);
    Erecs = pmax(Erecs,0); Ereca = pmax(Ereca,0);

    #stochastic version
    ESenter = rpois(num_loc,ESenter); ESleft = rpois(num_loc,ESleft);
    EEenter = rpois(num_loc, EEenter); EEleft = rpois(num_loc, EEleft);
    EIaenter = rpois(num_loc,EIaenter);EIaleft=rpois(num_loc,EIaleft);
    Eexps=rpois(num_loc,Eexps);
    Eexpa=rpois(num_loc,Eexpa);
    Einfs=rpois(num_loc,Einfs);
    Einfa=rpois(num_loc,Einfa);
    Erecs=rpois(num_loc,Erecs);
    Ereca=rpois(num_loc,Ereca);

    sk1=-Eexps-Eexpa+ESenter-ESleft;
    ek1=Eexps+Eexpa-Einfs-Einfa+EEenter-EEleft;
    isk1=Einfs-Erecs;
    iak1=Einfa-Ereca+EIaenter-EIaleft;
    ik1i=Einfs;

    #second step
    Ts1 = S[,tcnt]+ sk1/2;
    Te1 = E[,tcnt] + ek1/2;
    Tis1 = Is[,tcnt] + isk1/2;
    Tia1 = Ia[,tcnt] + iak1/2;


    ESenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (Ts1/(pop-Tis1)))
    ESleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Ts1/(pop-Tis1)) * colSums(M[[ts]]), dt1*Ts1)

    EEenter = dt1* (rep(1,num_loc)*theta) * (M[[ts]] %*% (Te1/(pop-Tis1)))
    EEleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Te1/(pop-Tis1)) * colSums(M[[ts]]), dt1*Te1)

    EIaenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (Tia1/(pop-Tis1)))
    EIaleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Tia1/(pop-Tis1)) * colSums(M[[ts]]), dt1*Tia1)


    Eexps = dt1*(rep(1,num_loc)*beta) * Ts1 * Tis1/pop;
    Eexpa = dt1*(rep(1,num_loc)*mu) * (rep(1,num_loc)*beta) * Ts1 * Tia1/pop;
    Einfs = dt1*(rep(1,num_loc)*alpha) * Te1/(rep(1,num_loc)*Z);
    Einfa = dt1*(rep(1,num_loc)*(1-alpha)) * Te1/(rep(1,num_loc)*Z);
    Erecs = dt1*Tis1/(rep(1,num_loc)*D);
    Ereca = dt1*Tia1/(rep(1,num_loc)*D);

    ESenter = pmax(ESenter,0); ESleft = pmax(ESleft,0)
    EEenter = pmax(EEenter,0); EEleft = pmax(EEleft,0);
    EIaenter = pmax(EIaenter,0); EIaleft= pmax(EIaleft,0);
    Eexps = pmax(Eexps,0); Eexpa = pmax(Eexpa,0);
    Einfs = pmax(Einfs,0); Einfa = pmax(Einfa,0);
    Erecs = pmax(Erecs,0); Ereca = pmax(Ereca,0);

    #stochastic version
    ESenter = rpois(num_loc,ESenter); ESleft = rpois(num_loc,ESleft);
    EEenter = rpois(num_loc, EEenter); EEleft = rpois(num_loc, EEleft);
    EIaenter = rpois(num_loc,EIaenter);EIaleft=rpois(num_loc,EIaleft);
    Eexps=rpois(num_loc,Eexps);
    Eexpa=rpois(num_loc,Eexpa);
    Einfs=rpois(num_loc,Einfs);
    Einfa=rpois(num_loc,Einfa);
    Erecs=rpois(num_loc,Erecs);
    Ereca=rpois(num_loc,Ereca);


    sk2=-Eexps-Eexpa+ESenter-ESleft;
    ek2=Eexps+Eexpa-Einfs-Einfa+EEenter-EEleft;
    isk2=Einfs-Erecs;
    iak2=Einfa-Ereca+EIaenter-EIaleft;
    ik2i=Einfs;


    # third step
    Ts2 = S[,tcnt]+ sk2/2;
    Te2 = E[,tcnt] + ek2/2;
    Tis2 = Is[,tcnt] + isk2/2;
    Tia2 = Ia[,tcnt] + iak2/2;


    ESenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (Ts2/(pop-Tis2)))
    ESleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Ts2/(pop-Tis2)) * colSums(M[[ts]]), dt1*Ts2)

    EEenter = dt1* (rep(1,num_loc)*theta) * (M[[ts]] %*% (Te2/(pop-Tis2)))
    EEleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Te2/(pop-Tis2)) * colSums(M[[ts]]), dt1*Te2)

    EIaenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (Tia2/(pop-Tis2)))
    EIaleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Tia2/(pop-Tis2)) * colSums(M[[ts]]), dt1*Tia2)


    Eexps = dt1*(rep(1,num_loc)*beta) * Ts2 * Tis2/pop;
    Eexpa = dt1*(rep(1,num_loc)*mu) * (rep(1,num_loc)*beta) * Ts2 * Tia2/pop;
    Einfs = dt1*(rep(1,num_loc)*alpha) * Te2/(rep(1,num_loc)*Z);
    Einfa = dt1*(rep(1,num_loc)*(1-alpha)) * Te2/(rep(1,num_loc)*Z);
    Erecs = dt1*Tis2/(rep(1,num_loc)*D);
    Ereca = dt1*Tia2/(rep(1,num_loc)*D);

    ESenter = pmax(ESenter,0); ESleft = pmax(ESleft,0)
    EEenter = pmax(EEenter,0); EEleft = pmax(EEleft,0);
    EIaenter = pmax(EIaenter,0); EIaleft= pmax(EIaleft,0);
    Eexps = pmax(Eexps,0); Eexpa = pmax(Eexpa,0);
    Einfs = pmax(Einfs,0); Einfa = pmax(Einfa,0);
    Erecs = pmax(Erecs,0); Ereca = pmax(Ereca,0);

    #stochastic version
    ESenter = rpois(num_loc,ESenter); ESleft = rpois(num_loc,ESleft);
    EEenter = rpois(num_loc, EEenter); EEleft = rpois(num_loc, EEleft);
    EIaenter = rpois(num_loc,EIaenter);EIaleft=rpois(num_loc,EIaleft);
    Eexps=rpois(num_loc,Eexps);
    Eexpa=rpois(num_loc,Eexpa);
    Einfs=rpois(num_loc,Einfs);
    Einfa=rpois(num_loc,Einfa);
    Erecs=rpois(num_loc,Erecs);
    Ereca=rpois(num_loc,Ereca);

    sk3=-Eexps-Eexpa+ESenter-ESleft;
    ek3=Eexps+Eexpa-Einfs-Einfa+EEenter-EEleft;
    isk3=Einfs-Erecs;
    iak3=Einfa-Ereca+EIaenter-EIaleft;
    ik3i=Einfs;

    # fourth step
    Ts3 = S[,tcnt]+sk3;
    Te3 = E[,tcnt]+ek3;
    Tis3 = Is[,tcnt]+isk3;
    Tia3 = Ia[,tcnt]+iak3;

    ESenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (Ts3/(pop-Tis3)))
    ESleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Ts3/(pop-Tis3)) * colSums(M[[ts]]), dt1*Ts3)

    EEenter = dt1* (rep(1,num_loc)*theta) * (M[[ts]] %*% (Te3/(pop-Tis3)))
    EEleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Te3/(pop-Tis3)) * colSums(M[[ts]]), dt1*Te3)

    EIaenter = dt1 * (rep(1,num_loc)*theta) * (M[[ts]] %*% (Tia3/(pop-Tis3)))
    EIaleft = pmin(dt1 * (rep(1, num_loc)*theta) * (Tia3/(pop-Tis3)) * colSums(M[[ts]]), dt1*Tia3)


    Eexps = dt1*(rep(1,num_loc)*beta) * Ts3 * Tis3/pop;
    Eexpa = dt1*(rep(1,num_loc)*mu) * (rep(1,num_loc)*beta) * Ts3 * Tia3/pop;
    Einfs = dt1*(rep(1,num_loc)*alpha) * Te3/(rep(1,num_loc)*Z);
    Einfa = dt1*(rep(1,num_loc)*(1-alpha)) * Te3/(rep(1,num_loc)*Z);
    Erecs = dt1*Tis3/(rep(1,num_loc)*D);
    Ereca = dt1*Tia3/(rep(1,num_loc)*D);

    ESenter = pmax(ESenter,0); ESleft = pmax(ESleft,0)
    EEenter = pmax(EEenter,0); EEleft = pmax(EEleft,0);
    EIaenter = pmax(EIaenter,0); EIaleft= pmax(EIaleft,0);
    Eexps = pmax(Eexps,0); Eexpa = pmax(Eexpa,0);
    Einfs = pmax(Einfs,0); Einfa = pmax(Einfa,0);
    Erecs = pmax(Erecs,0); Ereca = pmax(Ereca,0);

    #stochastic version
    ESenter = rpois(num_loc,ESenter); ESleft = rpois(num_loc,ESleft);
    EEenter = rpois(num_loc, EEenter); EEleft = rpois(num_loc, EEleft);
    EIaenter = rpois(num_loc,EIaenter);EIaleft=rpois(num_loc,EIaleft);
    Eexps=rpois(num_loc,Eexps);
    Eexpa=rpois(num_loc,Eexpa);
    Einfs=rpois(num_loc,Einfs);
    Einfa=rpois(num_loc,Einfa);
    Erecs=rpois(num_loc,Erecs);
    Ereca=rpois(num_loc,Ereca);


    sk4=-Eexps-Eexpa+ESenter-ESleft;
    ek4=Eexps+Eexpa-Einfs-Einfa+EEenter-EEleft;
    isk4=Einfs-Erecs;
    iak4=Einfa-Ereca+EIaenter-EIaleft;
    ik4i=Einfs;


    S[,tcnt+1] = S[,tcnt]+round(sk1/6+sk2/3+sk3/3+sk4/6);
    E[,tcnt+1] = E[,tcnt]+round(ek1/6+ek2/3+ek3/3+ek4/6);
    Is[,tcnt+1] = Is[,tcnt] + round(isk1/6+isk2/3+isk3/3+isk4/6);
    Ia[,tcnt+1] = Ia[,tcnt]+round(iak1/6+iak2/3+iak3/3+iak4/6);
    Incidence[,tcnt+1]=round(ik1i/6+ik2i/3+ik3i/3+ik4i/6);
    obs=Incidence[,tcnt+1];


  }

  # update x
  x[Sidx] = S[,tcnt+1];
  x[Eidx] = E[,tcnt+1];
  x[Isidx] = Is[,tcnt+1];
  x[Iaidx] = Ia[,tcnt+1];
  x[obsidx]=obs;

  #update pop
  pop = pop - colSums(M[[ts]])*theta + rowSums(M[[ts]])*theta;
  minfrac=0.6;
  pop[which(pop<minfrac*pop0)] = pop0[which(pop<minfrac*pop0)]*minfrac

  return(list(x=x, pop =pop))
}
