(*version August 22, 2016, correct #38*)
ode[[1]]=D[y[x],x]-(a4*x^4+a3*x^3+a2*x^2+a1*x+a0)^(-1/2)
ode[[2]]=D[y[x],x]+a*y[x]-c*Exp[b*x]
ode[[3]]=D[y[x],x]+a*y[x]-b*Sin[c*x]
ode[[4]]=D[y[x],x]+2*x*y[x]-x*Exp[-x^2]
ode[[5]]=D[y[x],x]+y[x]*Cos[x]-Exp[2*x]
ode[[6]]=D[y[x],x]+y[x]*Cos[x]-1/2*Sin[2*x]
ode[[7]]=D[y[x],x]+y[x]*Cos[x]-Exp[-Sin[x]]
ode[[8]]=D[y[x],x]+y[x]*Tan[x]-Sin[2*x]
ode[[9]]=D[y[x],x]-(Sin[Log[x]]+Cos[Log[x]]+a)*y[x]
ode[[10]]=D[y[x],x]+D[f[x],x]*y[x]-f[x]*D[f[x],x]
ode[[11]]=D[y[x],x]+f[x]*y[x]-g[x]
ode[[12]]=D[y[x],x]+y[x]^2-1
ode[[13]]=D[y[x],x]+y[x]^2-a*x-b
ode[[14]]=D[y[x],x]+y[x]^2+a*x^m
ode[[15]]=D[y[x],x]+y[x]^2-2*x^2*y[x]+x^4-2*x-1
ode[[16]]=D[y[x],x]+y[x]^2+(x*y[x]-1)*f[x]
ode[[17]]=D[y[x],x]-y[x]^2-3*y[x]+4
ode[[18]]=D[y[x],x]-y[x]^2-x*y[x]-x+1
ode[[19]]=D[y[x],x]-(y[x]+x)^2
ode[[20]]=D[y[x],x]-y[x]^2+(x^2+1)*y[x]-2*x
ode[[21]]=D[y[x],x]-y[x]^2+y[x]*Sin[x]-Cos[x]
ode[[22]]=D[y[x],x]-y[x]^2-y[x]*Sin[2*x]-Cos[2*x]
ode[[23]]=D[y[x],x]+a*y[x]^2-b
ode[[24]]=D[y[x],x]+a*y[x]^2-b*x^nu
ode[[25]]=D[y[x],x]+a*y[x]^2-b*x^(2*nu)-c*x^(nu-1)
ode[[26]]=D[y[x],x]-(A*y[x]-a)*(B*y[x]-b)
ode[[27]]=D[y[x],x]+a*y[x]*(y[x]-x)-1
ode[[28]]=D[y[x],x]+x*y[x]^2-x^3*y[x]-2*x
ode[[29]]=D[y[x],x]-x*y[x]^2-3*x*y[x]
ode[[30]]=D[y[x],x]+x^(-a-1)*y[x]^2-x^a
ode[[31]]=D[y[x],x]-a*x^n*(y[x]^2+1)
ode[[32]]=D[y[x],x]+y[x]^2*Sin[x]-2*Sin[x]/Cos[x]^2
ode[[33]]=D[y[x],x]-y[x]^2*D[f[x],x]/g[x]+D[g[x],x]/f[x]
ode[[34]]=D[y[x],x]+f[x]*y[x]^2+g[x]*y[x]
ode[[35]]=D[y[x],x]+f[x]*(y[x]^2+2*a*y[x]+b)
ode[[36]]=D[y[x],x]+y[x]^3+a*x*y[x]^2
ode[[37]]=D[y[x],x]-y[x]^3-a*Exp[x]*y[x]^2
ode[[38]]=D[y[x],x]-a*y[x]^3-b*x^(-3/2)
ode[[39]]=D[y[x],x]-a3*y[x]^3-a2*y[x]^2-a1*y[x]-a0
ode[[40]]=D[y[x],x]+3*a*y[x]^3+6*a*x*y[x]^2
ode[[41]]=D[y[x],x]+a*x*y[x]^3+b*y[x]^2
ode[[42]]=D[y[x],x]-x*(x+2)*y[x]^3-(x+3)*y[x]^2
ode[[43]]=D[y[x],x]+(3*a*x^2+4*a^2*x+b)*y[x]^3+3*x*y[x]^2
ode[[44]]=D[y[x],x]+2*a*x^3*y[x]^3+2*x*y[x]
ode[[45]]=D[y[x],x]+2*(a^2*x^3-b^2*x)*y[x]^3+3*b*y[x]^2
ode[[46]]=D[y[x],x]-x^a*y[x]^3+3*y[x]^2-x^(-a)*y[x]-x^(-2*a)+a*x^(-a-1)
ode[[47]]=D[y[x],x]-a*(x^n-x)*y[x]^3-y[x]^2
ode[[48]]=D[y[x],x]-(a*x^n+b*x)*y[x]^3-c*y[x]^2
ode[[49]]=D[y[x],x]+a*D[phi[x],x]*y[x]^3+6*a*phi[x]*y[x]^2+(2*a+1)*y[x]*D[D[phi[x],x],x]/D[phi[x],x]+2*a+2
ode[[50]]=D[y[x],x]-f3[x]*y[x]^3-f2[x]*y[x]^2-f1[x]*y[x]-f0[x]
ode[[51]]=D[y[x],x]-(y[x]-f[x])*(y[x]-g[x])*(y[x]-(a*f[x]+b*g[x])/(a+b))*h[x]-D[f[x],x]*(y[x]-g[x])/(f[x]-g[x])-D[g[x],x]*(y[x]-f[x])/(g[x]-f[x])
ode[[52]]=D[y[x],x]-a*y[x]^n-b*x^(n/(1-n))
ode[[53]]=D[y[x],x]-f[x]^(1-n)*D[g[x],x]*y[x]^n/(a*g[x]+b)^n-D[f[x],x]*y[x]/f[x]-f[x]*D[g[x],x]
ode[[54]]=D[y[x],x]-a^n*f[x]^(1-n)*D[g[x],x]*y[x]^n-D[f[x],x]*y[x]/f[x]-f[x]*D[g[x],x]
ode[[55]]=D[y[x],x]-f[x]*y[x]^n-g[x]*y[x]-h[x]
ode[[56]]=D[y[x],x]-f[x]*y[x]^a-g[x]*y[x]^b
ode[[57]]=D[y[x],x]-Abs[y[x]]^(1/2)
ode[[58]]=D[y[x],x]-a*y[x]^(1/2)-b*x
ode[[59]]=D[y[x],x]-a*(y[x]^2+1)^(1/2)-b
ode[[60]]=D[y[x],x]-(y[x]^2-1)^(1/2)/(x^2-1)^(1/2)
ode[[61]]=D[y[x],x]-(x^2-1)^(1/2)/(y[x]^2-1)^(1/2)
ode[[62]]=D[y[x],x]-(y[x]-x^2*(x^2-y[x]^2)^(1/2))/(x*y[x]*(x^2-y[x]^2)^(1/2)+x)
ode[[63]]=D[y[x],x]-(y[x]^2+1)/(Abs[y[x]+(1+y[x])^(1/2)]*(1+x)^(3/2))
ode[[64]]=D[y[x],x]-((a*y[x]^2+b*y[x]+c)/(a*x^2+b*x+c))^(1/2)
ode[[65]]=D[y[x],x]-((y[x]^3+1)/(x^3+1))^(1/2)
ode[[66]]=D[y[x],x]-Abs[y[x]*(1-y[x])*(1-a*y[x])]^(1/2)/Abs[x*(1-x)*(1-a*x)]^(1/2)
ode[[67]]=D[y[x],x]-(1-y[x]^4)^(1/2)/(1-x^4)^(1/2)
ode[[68]]=D[y[x],x]-((a*y[x]^4+b*y[x]^2+1)/(a*x^4+b*x^2+1))^(1/2)
ode[[69]]=D[y[x],x]-((b4*y[x]^4+b3*y[x]^3+b2*y[x]^2+b1*y[x]+b0)*(a4*x^4+a3*x^3+a2*x^2+a1*x+a0))^(1/2)
ode[[70]]=D[y[x],x]-((a4*x^4+a3*x^3+a2*x^2+a1*x+a0)/(b4*y[x]^4+b3*y[x]^3+b2*y[x]^2+b1*y[x]+b0))^(1/2)
ode[[71]]=D[y[x],x]-((b4*y[x]^4+b3*y[x]^3+b2*y[x]^2+b1*y[x]+b0)/(a4*x^4+a3*x^3+a2*x^2+a1*x+a0))^(1/2)
ode[[72]]=D[y[x],x]-R1[x,(a4*x^4+a3*x^3+a2*x^2+a1*x+a0)^(1/2)]*R2[y[x],(b4*y[x]^4+b3*y[x]^3+b2*y[x]^2+b1*y[x]+b0)^(1/2)]
ode[[73]]=D[y[x],x]-((a3*x^3+a2*x^2+a1*x+a0)/(a3*y[x]^3+a2*y[x]^2+a1*y[x]+a0))^(2/3)
ode[[74]]=D[y[x],x]-f[x]*(y[x]-g[x])*((y[x]-a)*(y[x]-b))^(1/2)
ode[[75]]=D[y[x],x]-Exp[x-y[x]]+Exp[x]
ode[[76]]=D[y[x],x]-a*Cos[y[x]]+b
ode[[77]]=D[y[x],x]-Cos[a*y[x]+b*x]
ode[[78]]=D[y[x],x]+a*Sin[alpha*y[x]+beta*x]+b
ode[[79]]=D[y[x],x]+f[x]*Cos[a*y[x]]+g[x]*Sin[a*y[x]]+h[x]
ode[[80]]=D[y[x],x]+f[x]*Sin[y[x]]+(1-D[f[x],x])*Cos[y[x]]-D[f[x],x]-1
ode[[81]]=D[y[x],x]+2*Tan[y[x]]*Tan[x]-1
ode[[82]]=D[y[x],x]-a*(1+Tan[y[x]]^2)+Tan[y[x]]*Tan[x]
ode[[83]]=D[y[x],x]-Tan[x*y[x]]
ode[[84]]=D[y[x],x]-f[a*x+b*y[x]]
ode[[85]]=D[y[x],x]-x^(a-1)*y[x]^(1-b)*f[x^a/a+y[x]^b/b]
ode[[86]]=D[y[x],x]-(y[x]-x*f[x^2+a*y[x]^2])/(x+a*y[x]*f[x^2+a*y[x]^2])
ode[[87]]=D[y[x],x]-(y[x]*a*f[x^c*y[x]]+c*x^a*y[x]^b)/(x*b*f[x^c*y[x]]-x^a*y[x]^b)
ode[[88]]=2*D[y[x],x]-3*y[x]^2-4*a*y[x]-b-c*Exp[-2*a*x]
ode[[89]]=x*D[y[x],x]-(a^2-x^2)^(1/2)
ode[[90]]=x*D[y[x],x]+y[x]-x*Sin[x]
ode[[91]]=x*D[y[x],x]-y[x]-x/Log[x]
ode[[92]]=x*D[y[x],x]-y[x]-x^2*Sin[x]
ode[[93]]=x*D[y[x],x]-y[x]-x*Cos[Log[Log[x]]]/Log[x]
ode[[94]]=x*D[y[x],x]+a*y[x]+b*x^n
ode[[95]]=x*D[y[x],x]+y[x]^2+x^2
ode[[96]]=x*D[y[x],x]-y[x]^2+1
ode[[97]]=x*D[y[x],x]+a*y[x]^2-y[x]+b*x^2
ode[[98]]=x*D[y[x],x]+a*y[x]^2-b*y[x]+c*x^(2*b)
ode[[99]]=x*D[y[x],x]+a*y[x]^2-b*y[x]-c*x^beta
ode[[100]]=x*D[y[x],x]+x*y[x]^2+a
ode[[101]]=x*D[y[x],x]+x*y[x]^2-y[x]
ode[[102]]=x*D[y[x],x]+x*y[x]^2-y[x]-a*x^3
ode[[103]]=x*D[y[x],x]+x*y[x]^2-(2*x^2+1)*y[x]-x^3
ode[[104]]=x*D[y[x],x]+a*x*y[x]^2+2*y[x]+b*x
ode[[105]]=x*D[y[x],x]+a*x*y[x]^2+b*y[x]+c*x+d
ode[[106]]=x*D[y[x],x]+x^a*y[x]^2+1/2*(a-b)*y[x]+x^b
ode[[107]]=x*D[y[x],x]+a*x^alpha*y[x]^2+b*y[x]-c*x^beta
ode[[108]]=x*D[y[x],x]-y[x]^2*Log[x]+y[x]
ode[[109]]=x*D[y[x],x]-y[x]*(2*y[x]*Log[x]-1)
ode[[110]]=x*D[y[x],x]+f[x]*(y[x]^2-x^2)
ode[[111]]=x*D[y[x],x]+y[x]^3+3*x*y[x]^2
ode[[112]]=x*D[y[x],x]-(y[x]^2+x^2)^(1/2)-y[x]
ode[[113]]=x*D[y[x],x]+a*(y[x]^2+x^2)^(1/2)-y[x]
ode[[114]]=x*D[y[x],x]-x*(y[x]^2+x^2)^(1/2)-y[x]
ode[[115]]=x*D[y[x],x]-x*(y[x]-x)*(y[x]^2+x^2)^(1/2)-y[x]
ode[[116]]=x*D[y[x],x]-x*((y[x]^2-x^2)*(y[x]^2-4*x^2))^(1/2)-y[x]
ode[[117]]=x*D[y[x],x]-x*Exp[y[x]/x]-y[x]-x
ode[[118]]=x*D[y[x],x]-y[x]*Log[y[x]]
ode[[119]]=x*D[y[x],x]-y[x]*(Log[x*y[x]]-1)
ode[[120]]=x*D[y[x],x]-y[x]*(x*Log[x^2/y[x]]+2)
ode[[121]]=x*D[y[x],x]+Sin[y[x]-x]
ode[[122]]=x*D[y[x],x]+(Sin[y[x]]-3*x^2*Cos[y[x]])*Cos[y[x]]
ode[[123]]=x*D[y[x],x]-x*Sin[y[x]/x]-y[x]
ode[[124]]=x*D[y[x],x]+x*Cos[y[x]/x]-y[x]+x
ode[[125]]=x*D[y[x],x]+x*Tan[y[x]/x]-y[x]
ode[[126]]=x*D[y[x],x]-y[x]*f[x*y[x]]
ode[[127]]=x*D[y[x],x]-y[x]*f[x^a*y[x]^b]
ode[[128]]=x*D[y[x],x]+a*y[x]-f[x]*g[x^a*y[x]]
ode[[129]]=(1+x)*D[y[x],x]+y[x]*(y[x]-x)
ode[[130]]=2*x*D[y[x],x]-y[x]-2*x^3
ode[[131]]=(2*x+1)*D[y[x],x]-4*Exp[-y[x]]+2
ode[[132]]=3*x*D[y[x],x]-3*x*Log[x]*y[x]^4-y[x]
ode[[133]]=x^2*D[y[x],x]+y[x]-x
ode[[134]]=x^2*D[y[x],x]-y[x]+x^2*Exp[x-1/x]
ode[[135]]=x^2*D[y[x],x]-(-1+x)*y[x]
ode[[136]]=x^2*D[y[x],x]+y[x]^2+x*y[x]+x^2
ode[[137]]=x^2*D[y[x],x]-y[x]^2-x*y[x]
ode[[138]]=x^2*D[y[x],x]-y[x]^2-x*y[x]-x^2
ode[[139]]=x^2*(D[y[x],x]+y[x]^2)+a*x^k-b*(b-1)
ode[[140]]=x^2*(D[y[x],x]+y[x]^2)+4*x*y[x]+2
ode[[141]]=x^2*(D[y[x],x]+y[x]^2)+a*x*y[x]+b
ode[[142]]=x^2*(D[y[x],x]-y[x]^2)-a*x^2*y[x]+a*x+2
ode[[143]]=x^2*(D[y[x],x]+a*y[x]^2)-b
ode[[144]]=x^2*(D[y[x],x]+a*y[x]^2)+b*x^alpha+c
ode[[145]]=x^2*D[y[x],x]+a*y[x]^3-a*x^2*y[x]^2
ode[[146]]=x^2*D[y[x],x]+x*y[x]^3+a*y[x]^2
ode[[147]]=x^2*D[y[x],x]+a*x^2*y[x]^3+b*y[x]^2
ode[[148]]=(x^2+1)*D[y[x],x]+x*y[x]-1
ode[[149]]=(x^2+1)*D[y[x],x]+x*y[x]-x*(x^2+1)
ode[[150]]=(x^2+1)*D[y[x],x]+2*x*y[x]-2*x^2
ode[[151]]=(x^2+1)*D[y[x],x]+(y[x]^2+1)*(2*x*y[x]-1)
ode[[152]]=(x^2+1)*D[y[x],x]+x*Sin[y[x]]*Cos[y[x]]-x*(x^2+1)*Cos[y[x]]^2
ode[[153]]=(x^2-1)*D[y[x],x]-x*y[x]+a
ode[[154]]=(x^2-1)*D[y[x],x]+2*x*y[x]-Cos[x]
ode[[155]]=(x^2-1)*D[y[x],x]+y[x]^2-2*x*y[x]+1
ode[[156]]=(x^2-1)*D[y[x],x]-y[x]*(y[x]-x)
ode[[157]]=(x^2-1)*D[y[x],x]+a*(y[x]^2-2*x*y[x]+1)
ode[[158]]=(x^2-1)*D[y[x],x]+a*x*y[x]^2+x*y[x]
ode[[159]]=(x^2-1)*D[y[x],x]-2*x*y[x]*Log[y[x]]
ode[[160]]=(x^2-4)*D[y[x],x]+(x+2)*y[x]^2-4*y[x]
ode[[161]]=(x^2-5*x+6)*D[y[x],x]+3*x*y[x]-8*y[x]+x^2
ode[[162]]=(x-a)*(x-b)*D[y[x],x]+y[x]^2+k*(y[x]+x-a)*(y[x]+x-b)
ode[[163]]=2*x^2*D[y[x],x]-2*y[x]^2-x*y[x]+2*a^2*x
ode[[164]]=2*x^2*D[y[x],x]-2*y[x]^2-3*x*y[x]+2*a^2*x
ode[[165]]=x*(2*x-1)*D[y[x],x]+y[x]^2-(4*x+1)*y[x]+4*x
ode[[166]]=2*x*(-1+x)*D[y[x],x]+(-1+x)*y[x]^2-x
ode[[167]]=3*x^2*D[y[x],x]-7*y[x]^2-3*x*y[x]-x^2
ode[[168]]=3*(x^2-4)*D[y[x],x]+y[x]^2-x*y[x]-3
ode[[169]]=(a*x+b)^2*D[y[x],x]+(a*x+b)*y[x]^3+c*y[x]^2
ode[[170]]=x^3*D[y[x],x]-y[x]^2-x^4
ode[[171]]=x^3*D[y[x],x]-y[x]^2-x^2*y[x]
ode[[172]]=x^3*D[y[x],x]-x^4*y[x]^2+x^2*y[x]+20
ode[[173]]=x^3*D[y[x],x]-x^6*y[x]^2-(2*x-3)*x^2*y[x]+3
ode[[174]]=x*(x^2+1)*D[y[x],x]+x^2*y[x]
ode[[175]]=x*(x^2-1)*D[y[x],x]-(2*x^2-1)*y[x]+a*x^3
ode[[176]]=x*(x^2-1)*D[y[x],x]+(x^2-1)*y[x]^2-x^2
ode[[177]]=x^2*(-1+x)*D[y[x],x]-y[x]^2-x*(x-2)*y[x]
ode[[178]]=2*x*(x^2-1)*D[y[x],x]+2*(x^2-1)*y[x]^2-(3*x^2-5)*y[x]+x^2-3
ode[[179]]=3*x*(x^2-1)*D[y[x],x]+x*y[x]^2-(x^2+1)*y[x]-3*x
ode[[180]]=(a*x^2+b*x+c)*(x*D[y[x],x]-y[x])-y[x]^2+x^2
ode[[181]]=x^4*(D[y[x],x]+y[x]^2)+a
ode[[182]]=x*(x^3-1)*D[y[x],x]-2*x*y[x]^2+y[x]+x^2
ode[[183]]=(2*x^4-x)*D[y[x],x]-2*(x^3-1)*y[x]
ode[[184]]=(a*x^2+b*x+c)^2*(D[y[x],x]+y[x]^2)+A
ode[[185]]=x^7*D[y[x],x]+2*(x^2+1)*y[x]^3+5*x^3*y[x]^2
ode[[186]]=x^n*D[y[x],x]+y[x]^2-(n-1)*x^(n-1)*y[x]+x^(2*n-2)
ode[[187]]=x^n*D[y[x],x]-a*y[x]^2-b*x^(2*n-2)
ode[[188]]=x^(2*n+1)*D[y[x],x]-a*y[x]^3-b*x^3*n
ode[[189]]=x^(m*(n-1)+n)*D[y[x],x]-a*y[x]^n-b*x^(n*(m+1))
ode[[190]]=(x^2-1)^(1/2)*D[y[x],x]-(y[x]^2-1)^(1/2)
ode[[191]]=(1-x^2)^(1/2)*D[y[x],x]-y[x]*(y[x]^2-1)^(1/2)
ode[[192]]=(x^2+a^2)^(1/2)*D[y[x],x]+y[x]-(x^2+a^2)^(1/2)+x
ode[[193]]=x*D[y[x],x]*Log[x]+y[x]-a*x*(Log[x]+1)
ode[[194]]=x*D[y[x],x]*Log[x]-y[x]^2*Log[x]-(2*Log[x]^2+1)*y[x]-Log[x]^3
ode[[195]]=Sin[x]*D[y[x],x]-y[x]^2*Sin[x]^2+(Cos[x]-3*Sin[x])*y[x]+4
ode[[196]]=Cos[x]*D[y[x],x]+y[x]+(1+Sin[x])*Cos[x]
ode[[197]]=Cos[x]*D[y[x],x]-y[x]^4-y[x]*Sin[x]
ode[[198]]=Sin[x]*Cos[x]*D[y[x],x]-y[x]-Sin[x]^3
ode[[199]]=Sin[2*x]*D[y[x],x]+Sin[2*y[x]]
ode[[200]]=(a*Sin[x]^2+b)*D[y[x],x]+a*y[x]*Sin[2*x]+A*x*(a*Sin[x]^2+c)
ode[[201]]=2*f[x]*D[y[x],x]+2*f[x]*y[x]^2-D[f[x],x]*y[x]-2*f[x]^2
ode[[202]]=f[x]*D[y[x],x]+g[x]*tg[y[x]]+h[x]
ode[[203]]=y[x]*D[y[x],x]+y[x]+x^3
ode[[204]]=y[x]*D[y[x],x]+a*y[x]+x
ode[[205]]=y[x]*D[y[x],x]+a*y[x]+1/4*(a^2-1)*x+b*x^n
ode[[206]]=y[x]*D[y[x],x]+a*y[x]+b*Exp[x]-2*a
ode[[207]]=y[x]*D[y[x],x]+y[x]^2+4*x*(1+x)
ode[[208]]=y[x]*D[y[x],x]+a*y[x]^2-b*Cos[x+c]
ode[[209]]=y[x]*D[y[x],x]-(a*y[x]^2+b)^(1/2)
ode[[210]]=y[x]*D[y[x],x]+x*y[x]^2-4*x
ode[[211]]=y[x]*D[y[x],x]-x*Exp[x/y[x]]
ode[[212]]=y[x]*D[y[x],x]+f[y[x]^2+x^2]*g[x]+x
ode[[213]]=(1+y[x])*D[y[x],x]-y[x]-x
ode[[214]]=(y[x]+x-1)*D[y[x],x]-y[x]+2*x+3
ode[[215]]=(y[x]+2*x-2)*D[y[x],x]-y[x]+x+1
ode[[216]]=(y[x]-2*x+1)*D[y[x],x]+y[x]+x
ode[[217]]=(y[x]-x^2)*D[y[x],x]-x
ode[[218]]=(y[x]-x^2)*D[y[x],x]+4*x*y[x]
ode[[219]]=(y[x]+g[x])*D[y[x],x]-f2[x]*y[x]^2-f1[x]*y[x]-f0[x]
ode[[220]]=2*y[x]*D[y[x],x]-x*y[x]^2-x^3
ode[[221]]=(2*y[x]+x+1)*D[y[x],x]-2*y[x]-x+1
ode[[222]]=(2*y[x]+x+7)*D[y[x],x]-y[x]+2*x+4
ode[[223]]=(2*y[x]-x)*D[y[x],x]-y[x]-2*x
ode[[224]]=(2*y[x]-6*x)*D[y[x],x]-y[x]+3*x+2
ode[[225]]=(4*y[x]+2*x+3)*D[y[x],x]-2*y[x]-x-1
ode[[226]]=(4*y[x]-2*x-3)*D[y[x],x]+2*y[x]-x-1
ode[[227]]=(4*y[x]-3*x-5)*D[y[x],x]-3*y[x]+7*x+2
ode[[228]]=(4*y[x]+11*x-11)*D[y[x],x]-25*y[x]-8*x+62
ode[[229]]=(12*y[x]-5*x-8)*D[y[x],x]-5*y[x]+2*x+3
ode[[230]]=a*y[x]*D[y[x],x]+b*y[x]^2+f[x]
ode[[231]]=(a*y[x]+b*x+c)*D[y[x],x]+alpha*y[x]+beta*x+EulerGamma
ode[[232]]=x*y[x]*D[y[x],x]+y[x]^2+x^2
ode[[233]]=x*y[x]*D[y[x],x]-y[x]^2+a*x^3*Cos[x]
ode[[234]]=x*y[x]*D[y[x],x]-y[x]^2+x*y[x]+x^3-2*x^2
ode[[235]]=(x*y[x]+a)*D[y[x],x]+b*y[x]
ode[[236]]=x*(y[x]+4)*D[y[x],x]-y[x]^2-2*y[x]-2*x
ode[[237]]=x*(y[x]+a)*D[y[x],x]+b*y[x]+c*x
ode[[238]]=(x*(y[x]+x)+a)*D[y[x],x]-y[x]*(y[x]+x)-b
ode[[239]]=(x*y[x]-x^2)*D[y[x],x]+y[x]^2-3*x*y[x]-2*x^2
ode[[240]]=2*x*y[x]*D[y[x],x]-y[x]^2+a*x
ode[[241]]=2*x*y[x]*D[y[x],x]-y[x]^2+a*x^2
ode[[242]]=2*x*y[x]*D[y[x],x]+2*y[x]^2+1
ode[[243]]=x*(2*y[x]+x-1)*D[y[x],x]-y[x]*(y[x]+2*x+1)
ode[[244]]=x*(2*y[x]-x-1)*D[y[x],x]+y[x]*(2*x-y[x]-1)
ode[[245]]=(2*x*y[x]+4*x^3)*D[y[x],x]+y[x]^2+112*x^2*y[x]
ode[[246]]=x*(3*y[x]+2*x)*D[y[x],x]+3*(y[x]+x)^2
ode[[247]]=(3*x+2)*(y[x]-2*x-1)*D[y[x],x]-y[x]^2+x*y[x]-7*x^2-9*x-3
ode[[248]]=(6*x*y[x]+x^2+3)*D[y[x],x]+3*y[x]^2+2*x*y[x]+2*x
ode[[249]]=(a*x*y[x]+b*x^n)*D[y[x],x]+alpha*y[x]^3+beta*y[x]^2
ode[[250]]=(B*x*y[x]+A*x^2+a*x+b*y[x]+c)*D[y[x],x]-B*y[x]^2+A*x*y[x]+alpha*x+beta*y[x]+EulerGamma
ode[[251]]=(x^2*y[x]-1)*D[y[x],x]+x*y[x]^2-1
ode[[252]]=(x^2*y[x]-1)*D[y[x],x]-x*y[x]^2+1
ode[[253]]=(x^2*y[x]-1)*D[y[x],x]+8*x*y[x]^2-8
ode[[254]]=x*(x*y[x]-2)*D[y[x],x]+x^2*y[x]^3+x*y[x]^2-2*y[x]
ode[[255]]=x*(x*y[x]-3)*D[y[x],x]+x*y[x]^2-y[x]
ode[[256]]=x^2*(-1+y[x])*D[y[x],x]+(-1+x)*y[x]
ode[[257]]=x*(x*y[x]+x^4-1)*D[y[x],x]-y[x]*(x*y[x]-x^4-1)
ode[[258]]=2*x^2*y[x]*D[y[x],x]+y[x]^2-2*x^3-x^2
ode[[259]]=2*x^2*y[x]*D[y[x],x]-y[x]^2-x^2*Exp[x-1/x]
ode[[260]]=(2*x^2*y[x]+x)*D[y[x],x]-x^2*y[x]^3+2*x*y[x]^2+y[x]
ode[[261]]=(2*x^2*y[x]-x)*D[y[x],x]-2*x*y[x]^2-y[x]
ode[[262]]=(2*x^2*y[x]-x^3)*D[y[x],x]+y[x]^3-4*x*y[x]^2+2*x^3
ode[[263]]=2*x^3+y[x]*D[y[x],x]+3*x^2*y[x]^2+7
ode[[264]]=2*x*(x^3*y[x]+1)*D[y[x],x]+(3*x^3*y[x]-1)*y[x]
ode[[265]]=(x^(n*(n+1))*y[x]-1)*D[y[x],x]+2*(n+1)^2*x^(n-1)*(x^(n^2)*y[x]^2-1)
ode[[266]]=(y[x]-x)*(x^2+1)^(1/2)*D[y[x],x]-a*((y[x]^2+1)^3)^(1/2)
ode[[267]]=y[x]*D[y[x],x]*Sin[x]^2+y[x]^2*Cos[x]*Sin[x]-1
ode[[268]]=f[x]*y[x]*D[y[x],x]+g[x]*y[x]^2+h[x]
ode[[269]]=(g1[x]*y[x]+g0[x])*D[y[x],x]-f1[x]*y[x]-f2[x]*y[x]^2-f3[x]*y[x]^3-f0[x]
ode[[270]]=(y[x]^2-x)*D[y[x],x]-y[x]+x^2
ode[[271]]=(y[x]^2+x^2)*D[y[x],x]+2*x*(y[x]+2*x)
ode[[272]]=(y[x]^2+x^2)*D[y[x],x]-y[x]^2
ode[[273]]=(y[x]^2+x^2+a)*D[y[x],x]+2*x*y[x]
ode[[274]]=(y[x]^2+x^2+a)*D[y[x],x]+2*x*y[x]+x^2+b
ode[[275]]=(y[x]^2+x^2+x)*D[y[x],x]-y[x]
ode[[276]]=(y[x]^2-x^2)*D[y[x],x]+2*x*y[x]
ode[[277]]=(y[x]^2+x^4)*D[y[x],x]-4*x^3*y[x]
ode[[278]]=(y[x]^2+4*Sin[x])*D[y[x],x]-Cos[x]
ode[[279]]=(y[x]^2+2*y[x]+x)*D[y[x],x]+(y[x]+x)^2*y[x]^2+y[x]*(1+y[x])
ode[[280]]=(y[x]+x)^2*D[y[x],x]-a^2
ode[[281]]=(y[x]^2+2*x*y[x]-x^2)*D[y[x],x]-y[x]^2+2*x*y[x]+x^2
ode[[282]]=(y[x]+3*x-1)^2*D[y[x],x]-(2*y[x]-1)*(4*y[x]+6*x-3)
ode[[283]]=3*(y[x]^2-x^2)*D[y[x],x]+2*y[x]^3-6*x*(1+x)*y[x]-3*Exp[x]
ode[[284]]=(4*y[x]^2+x^2)*D[y[x],x]-x*y[x]
ode[[285]]=(4*y[x]^2+2*x*y[x]+3*x^2)*D[y[x],x]+y[x]^2+6*x*y[x]+2*x^2
ode[[286]]=(2*y[x]-3*x+1)^2*D[y[x],x]-(3*y[x]-2*x-4)^2
ode[[287]]=(2*y[x]-4*x+1)^2*D[y[x],x]-(y[x]-2*x)^2
ode[[288]]=(6*y[x]^2-3*x^2*y[x]+1)*D[y[x],x]-3*x*y[x]^2+x
ode[[289]]=(6*y[x]-x)^2*D[y[x],x]-6*y[x]^2+2*x*y[x]+a
ode[[290]]=(a*y[x]^2+2*b*x*y[x]+c*x^2)*D[y[x],x]+b*y[x]^2+2*c*x*y[x]+d*x^2
ode[[291]]=(b*(beta*y[x]+alpha*x)^2-beta*(a*x+b*y[x]))*D[y[x],x]+a*(beta*y[x]+alpha*x)^2-alpha*(a*x+b*y[x])
ode[[292]]=(a*y[x]+b*x+c)^2*D[y[x],x]+(alpha*y[x]+beta*x+EulerGamma)^2
ode[[293]]=x*(y[x]^2-3*x)*D[y[x],x]+2*y[x]^3-5*x*y[x]
ode[[294]]=x*(y[x]^2+x^2-a)*D[y[x],x]-y[x]*(y[x]^2+x^2+a)
ode[[295]]=x*(y[x]^2+x*y[x]-x^2)*D[y[x],x]-y[x]^3+x*y[x]^2+x^2*y[x]
ode[[296]]=x*(y[x]^2+x^2*y[x]+x^2)*D[y[x],x]-2*y[x]^3-2*x^2*y[x]^2+x^4
ode[[297]]=2*x*(y[x]^2+5*x^2)*D[y[x],x]+y[x]^3-x^2*y[x]
ode[[298]]=3*x*y[x]^2*D[y[x],x]+y[x]^3-2*x
ode[[299]]=(3*x*y[x]^2-x^2)*D[y[x],x]+y[x]^3-2*x*y[x]
ode[[300]]=6*x*y[x]^2*D[y[x],x]+2*y[x]^3+x
ode[[301]]=(6*x*y[x]^2+x^2)*D[y[x],x]-y[x]*(3*y[x]^2-x)
ode[[302]]=(x^2*y[x]^2+x)*D[y[x],x]+y[x]
ode[[303]]=(x*y[x]-1)^2*x*D[y[x],x]+(x^2*y[x]^2+1)*y[x]
ode[[304]]=(10*x^3*y[x]^2+x^2*y[x]+2*x)*D[y[x],x]+5*x^2*y[x]^3+x*y[x]^2
ode[[305]]=(y[x]^3-3*x)*D[y[x],x]-3*y[x]+x^2
ode[[306]]=(y[x]^3-x^3)*D[y[x],x]-x^2*y[x]
ode[[307]]=(y[x]^2+x^2+a)*y[x]*D[y[x],x]+(y[x]^2+x^2-a)*x
ode[[308]]=2*y[x]^3*D[y[x],x]+x*y[x]^2
ode[[309]]=(2*y[x]^3+y[x])*D[y[x],x]-2*x^3-x
ode[[310]]=(2*y[x]^3+5*x^2*y[x])*D[y[x],x]+5*x*y[x]^2+x^3
ode[[311]]=(20*y[x]^3-3*x*y[x]^2+6*x^2*y[x]+3*x^3)*D[y[x],x]-y[x]^3+6*x*y[x]^2+9*x^2*y[x]+4*x^3
ode[[312]]=(y[x]^2/b+x^2/a)*(y[x]*D[y[x],x]+x)+(a-b)*(y[x]*D[y[x],x]-x)/(a+b)
ode[[313]]=(2*a*y[x]^3+3*a*x*y[x]^2-b*x^3+c*x^2)*D[y[x],x]-a*y[x]^3+c*y[x]^2+3*b*x^2*y[x]+2*b*x^3
ode[[314]]=x*y[x]^3*D[y[x],x]+y[x]^4-x*Sin[x]
ode[[315]]=(2*x*y[x]^3-x^4)*D[y[x],x]-y[x]^4+2*x^3*y[x]
ode[[316]]=(2*x*y[x]^3+y[x])*D[y[x],x]+2*y[x]^2
ode[[317]]=(2*x*y[x]^3+x*y[x]+x^2)*D[y[x],x]+y[x]^2-x*y[x]
ode[[318]]=(3*x*y[x]^3-4*x*y[x]+y[x])*D[y[x],x]+y[x]^2*(y[x]^2-2)
ode[[319]]=(7*x*y[x]^3+y[x]-5*x)*D[y[x],x]+y[x]^4-5*y[x]
ode[[320]]=(x^2*y[x]^3+x*y[x])*D[y[x],x]-1
ode[[321]]=(2*x^2*y[x]^3+x^2*y[x]^2-2*x)*D[y[x],x]-2*y[x]-1
ode[[322]]=(10*x^2*y[x]^3-3*y[x]^2-2)*D[y[x],x]+5*x*y[x]^4+x
ode[[323]]=(a*x*y[x]^3+c)*x*D[y[x],x]+(b*x^3*y[x]+c)*y[x]
ode[[324]]=(2*x^3*y[x]^3-x)*D[y[x],x]+2*x^3*y[x]^3-y[x]
ode[[325]]=y[x]*(y[x]^3-2*x^3)*D[y[x],x]+(2*y[x]^3-x^3)*x
ode[[326]]=y[x]*((a*y[x]+b*x)^3+b*x^3)*D[y[x],x]+x*((a*y[x]+b*x)^3+a*y[x]^3)
ode[[327]]=(x*y[x]^4+2*x^2*y[x]^3+2*y[x]+x)*D[y[x],x]+y[x]^5+y[x]
ode[[328]]=a*x^2*y[x]^n*D[y[x],x]-2*x*D[y[x],x]+y[x]
ode[[329]]=y[x]^m*x^n*(a*x*D[y[x],x]+b*y[x])+alpha*x*D[y[x],x]+beta*y[x]
ode[[330]]=(f[y[x]+x]+1)*D[y[x],x]+f[y[x]+x]
ode[[331]]=D[y[x],x]*Sum[f[nu][x]*y[x]^nu,{nu,1,p}]-Sum[g[nu][x]*y[x]^nu,{nu,1,q}]
ode[[332]]=((x*y[x])^(1/2)-1)*x*D[y[x],x]-((x*y[x])^(1/2)+1)*y[x]
ode[[333]]=(2*x^(5/2)*y[x]^(3/2)+x^2*y[x]-x)*D[y[x],x]-x^(3/2)*y[x]^(5/2)+x*y[x]^2-y[x]
ode[[334]]=((y[x]+x)^(1/2)+1)*D[y[x],x]+1
ode[[335]]=(y[x]^2-1)^(1/2)*D[y[x],x]-(x^2-1)^(1/2)
ode[[336]]=((y[x]^2+1)^(1/2)+a*x)*D[y[x],x]+(x^2+1)^(1/2)+a*y[x]
ode[[337]]=((y[x]^2+x^2)^(1/2)+x)*D[y[x],x]-y[x]
ode[[338]]=(y[x]*(y[x]^2+x^2)^(1/2)+(y[x]^2-x^2)*Sin[alpha]-2*x*y[x]*Cos[alpha])*D[y[x],x]+x*(y[x]^2+x^2)^(1/2)+2*x*y[x]*Sin[alpha]+(y[x]^2-x^2)*Cos[alpha]
ode[[339]]=(x*(x^2+y[x]^2+1)^(1/2)-y[x]*(y[x]^2+x^2))*D[y[x],x]-y[x]*(x^2+y[x]^2+1)^(1/2)-x*(y[x]^2+x^2)
ode[[340]]=(e1*(x+a)/((x+a)^2+y[x]^2)^(3/2)+e2*(x-a)/((x-a)^2+y[x]^2)^(3/2))*D[y[x],x]-y[x]*(e1/((x+a)^2+y[x]^2)^(3/2)+e2/((x-a)^2+y[x]^2)^(3/2))
ode[[341]]=(x*Exp[y[x]]+Exp[x])*D[y[x],x]+Exp[y[x]]+y[x]*Exp[x]
ode[[342]]=x*(3*Exp[x*y[x]]+2*Exp[-x*y[x]])*(x*D[y[x],x]+y[x])+1
ode[[343]]=(Log[y[x]]+x)*D[y[x],x]-1
ode[[344]]=(Log[y[x]]+2*x-1)*D[y[x],x]-2*y[x]
ode[[345]]=x*(2*x^2*y[x]*Log[y[x]]+1)*D[y[x],x]-2*y[x]
ode[[346]]=x*(y[x]*Log[x*y[x]]+y[x]-a*x)*D[y[x],x]-y[x]*(a*x*Log[x*y[x]]-y[x]+a*x)
ode[[347]]=D[y[x],x]*(1+Sin[x])*Sin[y[x]]+Cos[x]*(Cos[y[x]]-1)
ode[[348]]=(x*Cos[y[x]]+Sin[x])*D[y[x],x]+y[x]*Cos[x]+Sin[y[x]]
ode[[349]]=x*D[y[x],x]*Cot[y[x]/x]+2*x*Sin[y[x]/x]-y[x]*Cot[y[x]/x]
ode[[350]]=D[y[x],x]*Cos[y[x]]-Cos[x]*Sin[y[x]]^2-Sin[y[x]]
ode[[351]]=D[y[x],x]*Cos[y[x]]+x*Sin[y[x]]*Cos[y[x]]^2-Sin[y[x]]^3
ode[[352]]=D[y[x],x]*(Cos[y[x]]-Sin[alpha]*Sin[x])*Cos[y[x]]+(Cos[x]-Sin[alpha]*Sin[y[x]])*Cos[x]
ode[[353]]=x*D[y[x],x]*Cos[y[x]]+Sin[y[x]]
ode[[354]]=(x*Sin[y[x]]-1)*D[y[x],x]+Cos[y[x]]
ode[[355]]=(x*Cos[y[x]]+Cos[x])*D[y[x],x]-y[x]*Sin[x]+Sin[y[x]]
ode[[356]]=(x^2*Cos[y[x]]+2*y[x]*Sin[x])*D[y[x],x]+2*x*Sin[y[x]]+y[x]^2*Cos[x]
ode[[357]]=x*D[y[x],x]*Log[x]*Sin[y[x]]+Cos[y[x]]*(1-x*Cos[y[x]])
ode[[358]]=D[y[x],x]*Sin[y[x]]*Cos[x]+Cos[y[x]]*Sin[x]
ode[[359]]=3*D[y[x],x]*Sin[x]*Sin[y[x]]+5*Cos[x]^4*y[x]
ode[[360]]=D[y[x],x]*Cos[a*y[x]]-b*(1-c*Cos[a*y[x]])*(Cos[a*y[x]]^2-1+c*Cos[a*y[x]])^(1/2)
ode[[361]]=(x*Sin[x*y[x]]+Cos[y[x]+x]-Sin[y[x]])*D[y[x],x]+y[x]*Sin[x*y[x]]+Cos[y[x]+x]+Cos[x]
ode[[362]]=(x^2*y[x]*Sin[x*y[x]]-4*x)*D[y[x],x]+x*y[x]^2*Sin[x*y[x]]-y[x]
ode[[363]]=(x*D[y[x],x]-y[x])*Cos[y[x]/x]^2+x
ode[[364]]=(y[x]*Sin[y[x]/x]-x*Cos[y[x]/x])*x*D[y[x],x]-(x*Cos[y[x]/x]+y[x]*Sin[y[x]/x])*y[x]
ode[[365]]=(y[x]*f[y[x]^2+x^2]-x)*D[y[x],x]+y[x]+x*f[y[x]^2+x^2]
ode[[366]]=f[x^2+a*y[x]^2]*(a*y[x]*D[y[x],x]+x)-y[x]-x*D[y[x],x]
ode[[367]]=f[x^c*y[x]]*(b*x*D[y[x],x]-a)-x^a*y[x]^b*(x*D[y[x],x]+c*y[x])
ode[[368]]=D[y[x],x]^2+a*y[x]+b*x^2
ode[[369]]=D[y[x],x]^2+y[x]^2-a^2
ode[[370]]=D[y[x],x]^2+y[x]^2-f[x]^2
ode[[371]]=D[y[x],x]^2-y[x]^3+y[x]^2
ode[[372]]=D[y[x],x]^2-4*y[x]^3+a*y[x]+b
ode[[373]]=D[y[x],x]^2+a^2*y[x]^2*(Log[y[x]]^2-1)
ode[[374]]=D[y[x],x]^2-2*D[y[x],x]-y[x]^2
ode[[375]]=D[y[x],x]^2+a*D[y[x],x]+b*x
ode[[376]]=D[y[x],x]^2+a*D[y[x],x]+b*y[x]
ode[[377]]=D[y[x],x]^2+(x-2)*D[y[x],x]-y[x]+1
ode[[378]]=D[y[x],x]^2+(x+a)*D[y[x],x]-y[x]
ode[[379]]=D[y[x],x]^2-(1+x)*D[y[x],x]+y[x]
ode[[380]]=D[y[x],x]^2+2*x*D[y[x],x]-y[x]
ode[[381]]=D[y[x],x]^2-2*x*D[y[x],x]+y[x]
ode[[382]]=D[y[x],x]^2+a*x*D[y[x],x]-b*x^2-c
ode[[383]]=D[y[x],x]^2+a*x*D[y[x],x]+b*y[x]+c*x^2
ode[[384]]=D[y[x],x]^2+(a*x+b)*D[y[x],x]-a*y[x]+c
ode[[385]]=D[y[x],x]^2-2*x^2*D[y[x],x]+2*x*y[x]
ode[[386]]=D[y[x],x]^2+a*x^3*D[y[x],x]-2*a*x^2*y[x]
ode[[387]]=D[y[x],x]^2+(D[y[x],x]-y[x])*Exp[x]
ode[[388]]=D[y[x],x]^2-2*y[x]*D[y[x],x]-2*x
ode[[389]]=D[y[x],x]^2-(4*y[x]+1)*D[y[x],x]+(4*y[x]+1)*y[x]
ode[[390]]=D[y[x],x]^2+a*y[x]*D[y[x],x]-b*x-c
ode[[391]]=D[y[x],x]^2+(a*y[x]+b*x)*D[y[x],x]+a*b*x*y[x]
ode[[392]]=D[y[x],x]^2-x*y[x]*D[y[x],x]+y[x]^2*Log[a*y[x]]
ode[[393]]=D[y[x],x]^2+2*y[x]*D[y[x],x]*Cot[x]-y[x]^2
ode[[394]]=D[y[x],x]^2+2*f[x]*y[x]*D[y[x],x]+g[x]*y[x]^2-(g[x]-f[x]^2)*Exp[-2*Integrate[f[xp],{xp,a,x}]]
ode[[395]]=D[y[x],x]^2+2*f[x]*y[x]*D[y[x],x]+g[x]*y[x]^2+h[x]
ode[[396]]=D[y[x],x]^2+y[x]*(y[x]-x)*D[y[x],x]-x*y[x]^3
ode[[397]]=D[y[x],x]^2-2*x^3*y[x]^2*D[y[x],x]-4*x^2*y[x]^3
ode[[398]]=D[y[x],x]^2-3*x*y[x]^(2/3)*D[y[x],x]+9*y[x]^(5/3)
ode[[399]]=2*D[y[x],x]^2+(-1+x)*D[y[x],x]-y[x]
ode[[400]]=2*D[y[x],x]^2-2*x^2*D[y[x],x]+3*x*y[x]
ode[[401]]=3*D[y[x],x]^2-2*x*D[y[x],x]+y[x]
ode[[402]]=3*D[y[x],x]^2+4*x*D[y[x],x]-y[x]+x^2
ode[[403]]=a*D[y[x],x]^2+b*D[y[x],x]-y[x]
ode[[404]]=a*D[y[x],x]^2+b*x^2*D[y[x],x]+c*x*y[x]
ode[[405]]=a*D[y[x],x]^2+y[x]*D[y[x],x]-x
ode[[406]]=a*D[y[x],x]^2-y[x]*D[y[x],x]-x
ode[[407]]=x*D[y[x],x]^2-y[x]
ode[[408]]=x*D[y[x],x]^2-2*y[x]+x
ode[[409]]=x*D[y[x],x]^2-2*D[y[x],x]-y[x]
ode[[410]]=x*D[y[x],x]^2+4*D[y[x],x]-2*y[x]
ode[[411]]=x*D[y[x],x]^2+x*D[y[x],x]-y[x]
ode[[412]]=x*D[y[x],x]^2+y[x]*D[y[x],x]+a
ode[[413]]=x*D[y[x],x]^2+y[x]*D[y[x],x]-x^2
ode[[414]]=x*D[y[x],x]^2+y[x]*D[y[x],x]+x^3
ode[[415]]=x*D[y[x],x]^2+y[x]*D[y[x],x]-y[x]^4
ode[[416]]=x*D[y[x],x]^2+(y[x]-3*x)*D[y[x],x]+y[x]
ode[[417]]=x*D[y[x],x]^2-y[x]*D[y[x],x]+a
ode[[418]]=x*D[y[x],x]^2-y[x]*D[y[x],x]+a*y[x]
ode[[419]]=x*D[y[x],x]^2+2*y[x]*D[y[x],x]-x
ode[[420]]=x*D[y[x],x]^2-2*y[x]*D[y[x],x]+a
ode[[421]]=x*D[y[x],x]^2-2*y[x]*D[y[x],x]-x
ode[[422]]=x*D[y[x],x]^2-2*y[x]*D[y[x],x]+4*x
ode[[423]]=x*D[y[x],x]^2-2*y[x]*D[y[x],x]+2*y[x]+x
ode[[424]]=x*D[y[x],x]^2+a*y[x]*D[y[x],x]+b*x
ode[[425]]=(1+x)*D[y[x],x]^2-(y[x]+x)*D[y[x],x]+y[x]
ode[[426]]=(3*x+1)*D[y[x],x]^2-3*(y[x]+2)*D[y[x],x]+9
ode[[427]]=(3*x+5)*D[y[x],x]^2-(3*y[x]+x)*D[y[x],x]+y[x]
ode[[428]]=a*x*D[y[x],x]^2+(b*x-a*y[x]+c)*D[y[x],x]-b*y[x]
ode[[429]]=a*x*D[y[x],x]^2-(a*y[x]+b*x-a-b)*D[y[x],x]+b*y[x]
ode[[430]]=(a2*x+c2)*D[y[x],x]^2+(a1*x+b1*y[x]+c1)*D[y[x],x]+a0*x+b0*y[x]+c0
ode[[431]]=x^2*D[y[x],x]^2-y[x]^4+y[x]^2
ode[[432]]=(x*D[y[x],x]+a)^2-2*a*y[x]+x^2
ode[[433]]=(x*D[y[x],x]+y[x]+2*x)^2-4*x*y[x]-4*x^2-4*a
ode[[434]]=x^2*D[y[x],x]^2-2*x*y[x]*D[y[x],x]-x^2
ode[[435]]=x^2*D[y[x],x]^2-2*x*y[x]*D[y[x],x]+y[x]*(1+y[x])-x
ode[[436]]=x^2*D[y[x],x]^2-2*x*y[x]*D[y[x],x]+y[x]^2*(1-x^2)-x^4
ode[[437]]=x^2*D[y[x],x]^2-(2*x*y[x]+a)*D[y[x],x]+y[x]^2
ode[[438]]=x^2*D[y[x],x]^2+3*x*y[x]*D[y[x],x]+2*y[x]^2
ode[[439]]=x^2*D[y[x],x]^2+3*x*y[x]*D[y[x],x]+3*y[x]^2
ode[[440]]=x^2*D[y[x],x]^2+4*x*y[x]*D[y[x],x]-5*y[x]^2
ode[[441]]=x^2*D[y[x],x]^2-4*x*(y[x]+2)*D[y[x],x]+4*y[x]*(y[x]+2)
ode[[442]]=x^2*D[y[x],x]^2+(x^2*y[x]-2*x*y[x]+x^3)*D[y[x],x]+(y[x]^2-x^2*y[x])*(1-x)
ode[[443]]=x*(x*D[y[x],x]-y[x])^2-D[y[x],x]
ode[[444]]=x^2*D[y[x],x]^2-y[x]*(y[x]-2*x)*D[y[x],x]+y[x]^2
ode[[445]]=x^2*D[y[x],x]^2+(a*x^2*y[x]^3+b)*D[y[x],x]+a*b*y[x]^3
ode[[446]]=(x^2+1)*D[y[x],x]^2-2*x*y[x]*D[y[x],x]+y[x]^2-1
ode[[447]]=(x^2-1)*D[y[x],x]^2-1
ode[[448]]=(x^2-1)*D[y[x],x]^2-y[x]^2+1
ode[[449]]=(x^2-a^2)*D[y[x],x]^2+2*x*y[x]*D[y[x],x]+y[x]^2
ode[[450]]=(x^2-a^2)*D[y[x],x]^2-2*x*y[x]*D[y[x],x]-x^2
ode[[451]]=(x^2+a)*D[y[x],x]^2-2*x*y[x]*D[y[x],x]+y[x]^2+b
ode[[452]]=(2*x^2+1)*D[y[x],x]^2+(y[x]^2+2*x*y[x]+x^2+2)*D[y[x],x]+2*y[x]^2+1
ode[[453]]=(a^2-1)*x^2*D[y[x],x]^2+2*x*y[x]*D[y[x],x]-y[x]^2+a^2*x^2
ode[[454]]=a*x^2*D[y[x],x]^2-2*a*x*y[x]*D[y[x],x]+y[x]^2-a*(a-1)*x^2
ode[[455]]=x^3*D[y[x],x]^2+x^2*y[x]*D[y[x],x]+a
ode[[456]]=x*(x^2-1)*D[y[x],x]^2+2*(1-x^2)*y[x]*D[y[x],x]+x*y[x]^2-x
ode[[457]]=x^4*D[y[x],x]^2-x*D[y[x],x]-y[x]
ode[[458]]=x^2*(x^2-a^2)*D[y[x],x]^2-1
ode[[459]]=Exp[-2*x]*D[y[x],x]^2-(D[y[x],x]-1)^2+Exp[-2*y[x]]
ode[[460]]=(D[y[x],x]^2+y[x]^2)*Cos[x]^4-a^2
ode[[461]]=a[x]*D[y[x],x]^2+2*b[x]*y[x]*D[y[x],x]+c[x]*y[x]^2+2*d[x]*D[y[x],x]+2*e[x]*y[x]+f[x]
ode[[462]]=y[x]*D[y[x],x]^2-1
ode[[463]]=y[x]*D[y[x],x]^2-Exp[2*x]
ode[[464]]=y[x]*D[y[x],x]^2+2*x*D[y[x],x]-y[x]
ode[[465]]=y[x]*D[y[x],x]^2+2*x*D[y[x],x]-9*y[x]
ode[[466]]=y[x]*D[y[x],x]^2-2*x*D[y[x],x]+y[x]
ode[[467]]=y[x]*D[y[x],x]^2-4*x*D[y[x],x]+y[x]
ode[[468]]=y[x]*D[y[x],x]^2-4*a^2*x*D[y[x],x]+a^2*y[x]
ode[[469]]=y[x]*D[y[x],x]^2+a*x*D[y[x],x]+b*y[x]
ode[[470]]=y[x]*D[y[x],x]^2+x^3*D[y[x],x]-x^2*y[x]
ode[[471]]=y[x]*D[y[x],x]^2-(y[x]-x)*D[y[x],x]-x
ode[[472]]=(y[x]+x)*D[y[x],x]^2+2*x*D[y[x],x]-y[x]
ode[[473]]=(y[x]-2*x)*D[y[x],x]^2-2*(-1+x)*D[y[x],x]+y[x]-2
ode[[474]]=2*y[x]*D[y[x],x]^2-(4*x-5)*D[y[x],x]+2*y[x]
ode[[475]]=4*y[x]*D[y[x],x]^2+2*x*D[y[x],x]-y[x]
ode[[476]]=9*y[x]*D[y[x],x]^2+4*x^3*D[y[x],x]-4*x^2*y[x]
ode[[477]]=a*y[x]*D[y[x],x]^2+(2*x-b)*D[y[x],x]-y[x]
ode[[478]]=(a*y[x]+b)*(D[y[x],x]^2+1)-c
ode[[479]]=(b2*y[x]+a2*x+c2)*D[y[x],x]^2+(a1*x+b1*y[x]+c1)*D[y[x],x]+a0*x+b0*y[x]+c0
ode[[480]]=(a*y[x]-x^2)*D[y[x],x]^2+2*x*y[x]*D[y[x],x]^2-y[x]^2
ode[[481]]=x*y[x]*D[y[x],x]^2+(y[x]^2+x^2)*D[y[x],x]+x*y[x]
ode[[482]]=x*y[x]*D[y[x],x]^2+(x^22-y[x]^2+a)*D[y[x],x]-x*y[x]
ode[[483]]=(2*x*y[x]-x^2)*D[y[x],x]^2+2*x*y[x]*D[y[x],x]+2*x*y[x]-y[x]^2
ode[[484]]=(2*x*y[x]-x^2)*D[y[x],x]^2-6*x*y[x]*D[y[x],x]-y[x]^2+2*x*y[x]
ode[[485]]=a*x*y[x]*D[y[x],x]^2-(a*y[x]^2+b*x^2+c)*D[y[x],x]+b*x*y[x]
ode[[486]]=y[x]^2*D[y[x],x]^2+y[x]^2-a^2
ode[[487]]=y[x]^2*D[y[x],x]^2-6*x^3*D[y[x],x]+4*x^2*y[x]
ode[[488]]=y[x]^2*D[y[x],x]^2-4*a*y[x]*D[y[x],x]+y[x]^2-4*a*x+4*a^2
ode[[489]]=y[x]^2*D[y[x],x]^2+2*x*y[x]*D[y[x],x]+a*y[x]^2+b*x+c
ode[[490]]=y[x]^2*D[y[x],x]^2-2*x*y[x]*D[y[x],x]+2*y[x]^2-x^2+a
ode[[491]]=y[x]^2*D[y[x],x]^2+2*a*x*y[x]*D[y[x],x]+(1-a)*y[x]^2+a*x^2+(a-1)*b
ode[[492]]=(y[x]^2-a^2)*D[y[x],x]^2+y[x]^2
ode[[493]]=(y[x]^2-2*a*x+a^2)*D[y[x],x]^2+2*a*y[x]*D[y[x],x]+y[x]^2
ode[[494]]=(y[x]^2-a^2*x^2)*D[y[x],x]^2+2*x*y[x]*D[y[x],x]+(1-a^2)*x^2
ode[[495]]=(y[x]^2+(1-a)*x^2)*D[y[x],x]^2+2*a*x*y[x]*D[y[x],x]+(1-a)*y[x]^2+x^2
ode[[496]]=(y[x]-x)^2*(D[y[x],x]^2+1)-a^2*(D[y[x],x]+1)^2
ode[[497]]=3*y[x]^2*D[y[x],x]^2-2*x*y[x]*D[y[x],x]+4*y[x]^2-x^2
ode[[498]]=(3*y[x]-2)*D[y[x],x]^2+4*y[x]-4
ode[[499]]=(1-a^2)*y[x]^2*D[y[x],x]^2-2*a^2*x*y[x]*D[y[x],x]+y[x]^2-a^2*x^2
ode[[500]]=(a-b)*y[x]^2*D[y[x],x]^2-2*b*x*y[x]*D[y[x],x]+a*y[x]^2-b*x^2-a*b
ode[[501]]=(a*y[x]^2+b*x+c)*D[y[x],x]^2-b*y[x]*D[y[x],x]+d*y[x]^2
ode[[502]]=(a*y[x]-b*x)^2*(a^2*D[y[x],x]^2+b^2)-c^2*(a*D[y[x],x]+b)^2
ode[[503]]=(b2*y[x]+a2*x+c2)^2*D[y[x],x]^2+(a1*x+b1*y[x]+c1)*D[y[x],x]+b0*y[x]+a0+c0
ode[[504]]=x*y[x]^2*D[y[x],x]^2-(y[x]^3+x^3-a)*D[y[x],x]+x^2*y[x]
ode[[505]]=x*y[x]^2*D[y[x],x]^2-2*y[x]^3*D[y[x],x]+2*x*y[x]^2-x^3
ode[[506]]=x^2*(x*y[x]^2-1)*D[y[x],x]^2+2*x^2*y[x]^2*(y[x]-x)*D[y[x],x]-y[x]^2*(x^2*y[x]-1)
ode[[507]]=(y[x]^4-a^2*x^2)*D[y[x],x]^2+2*a^2*x*y[x]*D[y[x],x]+y[x]^2*(y[x]^2-a^2)
ode[[508]]=(y[x]^4+x^2*y[x]^2-x^2)*D[y[x],x]^2+2*x*y[x]*D[y[x],x]-y[x]^2
ode[[509]]=9*y[x]^4*(x^2-1)*D[y[x],x]^2-6*x*y[x]^5*D[y[x],x]-4*x^2
ode[[510]]=x^2*(x^2*y[x]^4-1)*D[y[x],x]^2+2*x^3*y[x]^3*(y[x]^2-x^2)*D[y[x],x]-y[x]^2*(x^4*y[x]^2-1)
ode[[511]]=(a^2*(y[x]^2+x^2)^(1/2)-x^2)*D[y[x],x]^2+2*x*y[x]*D[y[x],x]+a^2*(y[x]^2+x^2)^(1/2)-y[x]^2
ode[[512]]=(a*(y[x]^2+x^2)^(3/2)-x^2)*D[y[x],x]^2+2*x*y[x]*D[y[x],x]+a*(y[x]^2+x^2)^(3/2)-y[x]^2
ode[[513]]=D[y[x],x]^2*Sin[y[x]]+2*x*D[y[x],x]*Cos[y[x]]^3-Sin[y[x]]*Cos[y[x]]^4
ode[[514]]=D[y[x],x]^2*(a*Cos[y[x]]+b)-c*Cos[y[x]]+d
ode[[515]]=f[y[x]^2+x^2]*(D[y[x],x]^2+1)-(x*D[y[x],x]-y[x])^2
ode[[516]]=(y[x]^2+x^2)*f[x/(y[x]^2+x^2)^(1/2)]*(D[y[x],x]^2+1)-(x*D[y[x],x]-y[x])^2
ode[[517]]=(y[x]^2+x^2)*f[y[x]/(y[x]^2+x^2)^(1/2)]*(D[y[x],x]^2+1)-(x*D[y[x],x]-y[x])^2
ode[[518]]=D[y[x],x]^3-(y[x]-a)^2*(y[x]-b)^2
ode[[519]]=D[y[x],x]^3-f[x]*(a*y[x]^2+b*y[x]+c)^2
ode[[520]]=D[y[x],x]^3+D[y[x],x]-y[x]
ode[[521]]=D[y[x],x]^3+x*D[y[x],x]-y[x]
ode[[522]]=D[y[x],x]^3-(x+5)*D[y[x],x]+y[x]
ode[[523]]=D[y[x],x]^3-a*x*D[y[x],x]+x^3
ode[[524]]=D[y[x],x]^3-2*y[x]*D[y[x],x]+y[x]^2
ode[[525]]=D[y[x],x]^2-a*x*y[x]*D[y[x],x]+2*a*y[x]^2
ode[[526]]=D[y[x],x]^3-(y[x]^2+x*y[x]+x^2)*D[y[x],x]^2+(x*y[x]^3+x^2*y[x]^2+x^3*y[x])*D[y[x],x]-x^3*y[x]^3
ode[[527]]=D[y[x],x]^3-x*y[x]^4*D[y[x],x]-y[x]^5
ode[[528]]=D[y[x],x]^3+a*D[y[x],x]^2+b*y[x]+a*b*x
ode[[529]]=D[y[x],x]^3+x*D[y[x],x]^2-y[x]
ode[[530]]=D[y[x],x]^3-y[x]*D[y[x],x]^2+y[x]^2
ode[[531]]=D[y[x],x]^2-(y[x]^4+x*y[x]^2+x^2)*D[y[x],x]^2+(x*y[x]^6+x^2*y[x]^4+x^3*y[x]^2)*D[y[x],x]-x^3*y[x]^6
ode[[532]]=a*D[y[x],x]^3+b*D[y[x],x]^2+c*D[y[x],x]-y[x]-d
ode[[533]]=x*D[y[x],x]^3-y[x]*D[y[x],x]^2+a
ode[[534]]=4*x*D[y[x],x]^3-6*y[x]*D[y[x],x]^2+3*y[x]-x
ode[[535]]=8*x*D[y[x],x]^3-12*y[x]*D[y[x],x]^2+9*y[x]
ode[[536]]=(x^2-a^2)*D[y[x],x]^3+b*x*(x^2-a^2)*D[y[x],x]^2+D[y[x],x]+b*x
ode[[537]]=x^3*D[y[x],x]^3-3*x^2*y[x]*D[y[x],x]^2+(3*x*y[x]^2+x^6)*D[y[x],x]-y[x]^3-2*x^5*y[x]
ode[[538]]=2*(x*D[y[x],x]+y[x])^3-y[x]*D[y[x],x]
ode[[539]]=D[y[x],x]^3*Sin[x]-(y[x]*Sin[x]-Cos[x]^2)*D[y[x],x]^2-(y[x]*Cos[x]^2+Sin[x])*D[y[x],x]+y[x]*Sin[x]
ode[[540]]=2*y[x]*D[y[x],x]^3-y[x]*D[y[x],x]^2+2*x*D[y[x],x]-x
ode[[541]]=y[x]^2*D[y[x],x]^3+2*x*D[y[x],x]-y[x]
ode[[542]]=16*y[x]^2*D[y[x],x]^3+2*x*D[y[x],x]-y[x]
ode[[543]]=x*y[x]^2*D[y[x],x]^3-y[x]^3*D[y[x],x]^2+x*(x^2+1)*D[y[x],x]-x^2*y[x]
ode[[544]]=x^7*y[x]^2*D[y[x],x]^3-(3*x^6*y[x]^3-1)*D[y[x],x]^2+3*x^5*y[x]^4*D[y[x],x]-x^4*y[x]^5
ode[[545]]=D[y[x],x]^4-(y[x]-a)^3*(y[x]-b)^2
ode[[546]]=D[y[x],x]^4+3*(-1+x)*D[y[x],x]^2-3*(2*y[x]-1)*D[y[x],x]+3*x
ode[[547]]=D[y[x],x]^4-4*y[x]*(x*D[y[x],x]-2*y[x])^2
ode[[548]]=D[y[x],x]^6-(y[x]-a)^4*(y[x]-b)^3
ode[[549]]=x^2*(D[y[x],x]^2+1)^3-a^2
ode[[550]]=D[y[x],x]^r-a*y[x]^s-b*x^(r*s/(r-s))
ode[[551]]=D[y[x],x]^n-f[x]^n*(y[x]-a)^(n+1)*(y[x]-b)^(n-1)
ode[[552]]=D[y[x],x]^n-f[x]*g[y[x]]
ode[[553]]=a*D[y[x],x]^m+b*D[y[x],x]^n-y[x]
ode[[554]]=x^(n-1)*D[y[x],x]^n-n*x*D[y[x],x]+y[x]
ode[[555]]=(D[y[x],x]^2+1)^(1/2)+x*D[y[x],x]-y[x]
ode[[556]]=(D[y[x],x]^2+1)^(1/2)+x*D[y[x],x]^2+y[x]
ode[[557]]=x*((D[y[x],x]^2+1)^(1/2)+D[y[x],x])-y[x]
ode[[558]]=a*x*(D[y[x],x]^2+1)^(1/2)+x*D[y[x],x]-y[x]
ode[[559]]=y[x]*(D[y[x],x]^2+1)^(1/2)-a*y[x]*D[y[x],x]-a*x
ode[[560]]=a*y[x]*(D[y[x],x]^2+1)^(1/2)-2*x*y[x]*D[y[x],x]+y[x]^2-x^2
ode[[561]]=f[y[x]^2+x^2]*(D[y[x],x]^2+1)^(1/2)-x*D[y[x],x]+y[x]
ode[[562]]=a*(D[y[x],x]^3+1)^(1/3)+b*x*D[y[x],x]-y[x]
ode[[563]]=Log[D[y[x],x]]+x*D[y[x],x]+a*y[x]+b
ode[[564]]=Log[D[y[x],x]]+a*(x*D[y[x],x]-y[x])
ode[[565]]=y[x]*Log[D[y[x],x]]+D[y[x],x]-y[x]*Log[y[x]]-x*y[x]
ode[[566]]=Sin[D[y[x],x]]+D[y[x],x]-x
ode[[567]]=a*Cos[D[y[x],x]]+b*D[y[x],x]+x
ode[[568]]=D[y[x],x]^2*Sin[D[y[x],x]]-y[x]
ode[[569]]=(D[y[x],x]^2+1)*Sin[x*D[y[x],x]-y[x]]^2-1
ode[[570]]=(D[y[x],x]^2+1)*(ArcTan[D[y[x],x]]+a*x)+D[y[x],x]
ode[[571]]=a*x^n*f[D[y[x],x]]+x*D[y[x],x]-y[x]
ode[[572]]=(x*D[y[x],x]-y[x])^n*f[D[y[x],x]]+y[x]*g[D[y[x],x]]+x*h[D[y[x],x]]
ode[[573]]=f[x*D[y[x],x]^2]+2*x*D[y[x],x]-y[x]
ode[[574]]=f[x-3/2*D[y[x],x]^2]+D[y[x],x]^3-y[x]
ode[[575]]=D[y[x],x]*f[x*y[x]*D[y[x],x]-y[x]^2]-x^2*D[y[x],x]+x*y[x]
ode[[576]]=phi[f[x,y[x],D[y[x],x]],g[x,y[x],D[y[x],x]]]
(*chapter 2*)
ode[[1001]]=(y''[x]==0)
ode[[1002]]=(y[x]+y''[x]==0)
ode[[1003]]=(-Sin[n x]+y[x]+y''[x]==0)
ode[[1004]]=(-a Cos[b x]+y[x]+y''[x]==0)
ode[[1005]]=(-Sin[a x] Sin[b x]+y[x]+y''[x]==0)
ode[[1006]]=(-y[x]+y''[x]==0)
ode[[1007]]=(-4 E^x^2 x^2-2 y[x]+y''[x]==0)
ode[[1008]]=(-Cot[a x]+a^2 y[x]+y''[x]==0)
ode[[1009]]=(l y[x]+y''[x]==0)
ode[[1010]]=((b+a x) y[x]+y''[x]==0)
ode[[1011]]=(-(1+x^2) y[x]+y''[x]==0)
ode[[1012]]=(-(a+x^2) y[x]+y''[x]==0)
ode[[1013]]=(-(a+a^2 x^2) y[x]+y''[x]==0)
ode[[1014]]=(-c x^a y[x]+y''[x]==0)
ode[[1015]]=(-(-1+a^2 x^(2 n)) y[x]+y''[x]==0)
ode[[1016]]=((b x^(-1+c)+a x^(2 c)) y[x]+y''[x]==0)
ode[[1017]]=((E^(2 x)-v^2) y[x]+y''[x]==0)
ode[[1018]]=(a E^(b x) y[x]+y''[x]==0)
ode[[1019]]=(-(-1+4 a^2 b^2 E^(2 b x^2) x^2) y[x]+y''[x]==0)
ode[[1020]]=((c+b E^x+a E^(2 x)) y[x]+y''[x]==0)
ode[[1021]]=((b+a Cos[x]^2) y[x]+y''[x]==0)
ode[[1022]]=((b+a Cos[2 x]) y[x]+y''[x]==0)
ode[[1023]]=((b+a Cos[x]^2) y[x]+y''[x]==0)
ode[[1024]]=(-(1+2 Tan[x]^2) y[x]+y''[x]==0)
ode[[1025]]=(-(a+(-1+n) n Csc[x]^2+(-1+m) m Sec[x]^2) y[x]+y''[x]==0)
ode[[1026]]=(y''[x]-(n*(n+1)*WeierstrassP[x,{g2,g3}]+B)*y[x]==0)
ode[[1027]]=((b+a JacobiSN[x,k]^2) y[x]+y''[x]==0)
ode[[1028]]=(y''[x]-y[x] (b+a p[x]+(7 p''[x])/3+1/30 (p^(4))[x])==0)
ode[[1029]]=(-y[x] (f[x]^2+f'[x])+y''[x]==0)
ode[[1030]]=((l+P[x]) y[x]+y''[x]==0)
ode[[1031]]=(-f[x] y[x]+y''[x]==0)
ode[[1032]]=(y''[x]+y[x] (g'[x]^2+((1/4-v^2) g'[x]^2)/g[x]-(3 g''[x]^2)/(4 g'[x]^2)+(g^(3))[x]/(2 g'[x]))==0)
ode[[1033]]=(a E^(-2 x) y[x]+y'[x]+y''[x]==0)
ode[[1034]]=(E^(2 x) y[x]-y'[x]+y''[x]==0)
ode[[1035]]=(b y[x]+a y'[x]+y''[x]==0)
ode[[1036]]=(-f[x]+b y[x]+a y'[x]+y''[x]==0)
ode[[1037]]=(-(c+b^2 x^2) y[x]+a y'[x]+y''[x]==0)
ode[[1038]]=(f[x] y[x]+2 a y'[x]+y''[x]==0)
ode[[1039]]=(y[x]+x y'[x]+y''[x]==0)
ode[[1040]]=(-y[x]+x y'[x]+y''[x]==0)
ode[[1041]]=((1+n) y[x]+x y'[x]+y''[x]==0)
ode[[1042]]=(-n y[x]+x y'[x]+y''[x]==0)
ode[[1043]]=(2 y[x]-x y'[x]+y''[x]==0)
ode[[1044]]=(-a y[x]-x y'[x]+y''[x]==0)
ode[[1045]]=((-1+x) y[x]-x y'[x]+y''[x]==0)
ode[[1046]]=(a y[x]-2 x y'[x]+y''[x]==0)
ode[[1047]]=((2+4 x^2) y[x]+4 x y'[x]+y''[x]==0)
ode[[1048]]=((-1+2 n+3 x^2) y[x]-4 x y'[x]+y''[x]==0)
ode[[1049]]=(-E^x+(-1+4 x^2) y[x]-4 x y'[x]+y''[x]==0)
ode[[1050]]=((-2+4 x^2) y[x]-4 x y'[x]+y''[x]==0)
ode[[1051]]=(-E^x^2+(-3+4 x^2) y[x]-4 x y'[x]+y''[x]==0)
ode[[1052]]=(b y[x]+a x y'[x]+y''[x]==0)
ode[[1053]]=(a^2 x^2 y[x]+2 a x y'[x]+y''[x]==0)
ode[[1054]]=((d+c x) y[x]+(b+a x) y'[x]+y''[x]==0)
ode[[1055]]=((c1+b1 x+a1 x^2) y[x]+(b+a x) y'[x]+y''[x]==0)
ode[[1056]]=(x y[x]-x^2 y'[x]+y''[x]==0)
ode[[1057]]=(-(1+x)^2 y[x]-x^2 y'[x]+y''[x]==0)
ode[[1058]]=(x (-2+x^4) y[x]-x^2 (1+x) y'[x]+y''[x]==0)
ode[[1059]]=(-x^3 y[x]+x^4 y'[x]+y''[x]==0)
ode[[1060]]=(b x^(-2+q) y[x]+a x^(-1+q) y'[x]+y''[x]==0)
ode[[1061]]=(-E^(-(x^(3/2)/3)) x+(-9+1/(4 Sqrt[x])+x/4) y[x]+Sqrt[x] y'[x]+y''[x]==0)
ode[[1062]]=(((-8+Sqrt[x]+x) y[x])/(4 x^2)-y'[x]/Sqrt[x]+y''[x]==0)
ode[[1063]]=(-E^(3 x)+E^(2 x) y[x]-(1+2 E^x) y'[x]+y''[x]==0)
ode[[1064]]=(Tan[x]+b y[x]+a y'[x]+y''[x]==0)
ode[[1065]]=((-a^2+n^2) y[x]+2 n Cot[x] y'[x]+y''[x]==0)
ode[[1066]]=(Cos[x]^2 y[x]+Tan[x] y'[x]+y''[x]==0)
ode[[1067]]=(-Cos[x]^2 y[x]+Tan[x] y'[x]+y''[x]==0)
ode[[1068]]=(v (1+v) y[x]+Cot[x] y'[x]+y''[x]==0)
ode[[1069]]=(Sin[x]^2 y[x]-Cot[x] y'[x]+y''[x]==0)
ode[[1070]]=(b y[x]+a Tan[x] y'[x]+y''[x]==0)
ode[[1071]]=((-a^2+b^2) y[x]+2 a Cot[a x] y'[x]+y''[x]==0)
ode[[1072]]=((a+b p[x]-4 a n p[x]^2) y[x]+a y'[x] p''[x]+y''[x]==0)
ode[[1073]]=(y''[x]+(WeierstrassP[x,{a,b}]^3-WeierstrassP[x,{a,b}]*D[WeierstrassP[x,{a,b}],x]-D[WeierstrassP[x,{a,b}],{x,2}])/(D[WeierstrassP[x,{a,b}],x]-WeierstrassP[x,{a,b}]^2)y'[x]+(D[WeierstrassP[x,{a,b}],x]^2-WeierstrassP[x,{a,b}]^2*D[WeierstrassP[x,{a,b}],x]-WeierstrassP[x,{a,b}]*D[WeierstrassP[x,{a,b}],{x,2}])/(D[WeierstrassP[x,{a,b}],x]+WeierstrassP[x,{a,b}]^2)y[x]==0)
ode[[1074]]=(n^2 JacobiDN[x,k]^2 y[x]+(k^2 JacobiCN[x,k] JacobiSN[x,k] y'[x])/JacobiDN[x,k]+y''[x]==0)
ode[[1075]]=(g[x] y[x]+f[x] y'[x]+y''[x]==0)
ode[[1076]]=(-g[x]+y[x] (a+f'[x])+f[x] y'[x]+y''[x]==0)
ode[[1077]]=((d+c f[x]) y[x]+(b+a f[x]) y'[x]+y''[x]==0)
ode[[1078]]=(y[x] (a+f[x]^2/4+f'[x]/2)+f[x] y'[x]+y''[x]==0)
ode[[1079]]=(b f[x]^(2 a) y[x]-(a f'[x] y'[x])/f[x]+y''[x]==0)
ode[[1080]]=(y[x] (a^2-b^2 f[x]^2+(a f'[x])/f[x])-(2 a+f'[x]/f[x]) y'[x]+y''[x]==0)
ode[[1081]]=(-((a^2 y[x] f'[x]^2)/(b^2+f[x]^2))+y''[x]+(f[x] y'[x] (f^(3))[x])/(b^2+f[x]^2)==0)
ode[[1082]]=(y[x] (g'[x]^2+((m^2-v^2) g'[x]^2)/g[x])-y'[x] (((-1+2 m) g'[x])/g[x]+g''[x]/g'[x])+y''[x]==0)
ode[[1083]]=(-((f'[x] y'[x])/f[x])+y''[x]+y[x] ((3 f'[x]^2)/(4 f[x]^2)+g'[x]^2+((1/4-v^2) g'[x]^2)/g[x]^2-f''[x]/(2 f[x])-(3 g''[x]^2)/(4 g'[x]^2)+(g^(3))[x]/(2 g'[x]))==0)
ode[[1084]]=(-y'[x] ((2 f'[x])/f[x]-g'[x]/g[x]+g''[x]/g'[x])+y[x] (g'[x]^2-(v^2 g'[x]^2)/g[x]^2-f''[x]/f[x]+(f'[x] ((2 f'[x])/f[x]-g'[x]/g[x]+g''[x]/g'[x]))/f[x])+y''[x]==0)
ode[[1085]]=(-y'[x] (((-1+2 v) g'[x])/g[x]+(2 h'[x])/h[x]+g''[x]/g'[x])+y[x] (g'[x]^2+(h'[x] (((-1+2 v) g'[x])/g[x]+(2 h'[x])/h[x]+g''[x]/g'[x]))/h[x]-h''[x]/h[x])+y''[x]==0)
ode[[1086]]=(9 x y[x]+4 y''[x]==0)
ode[[1087]]=(-(a+x^2) y[x]+4 y''[x]==0)
ode[[1088]]=(-(2+5 Tan[x]^2) y[x]+4 Tan[x] y'[x]+4 y''[x]==0)
ode[[1089]]=((d+b (c+x)) y[x]-(a b+c+x) y'[x]+a y''[x]==0)
ode[[1090]]=(b^2 E^(-2 a x) y[x]+a (a^2-2 b E^(-a x)) y'[x]+a^2 y''[x]==0)
ode[[1091]]=(-Cos[x]+x (y[x]+y''[x])==0)
ode[[1092]]=((a+x) y[x]+x y''[x]==0)
ode[[1093]]=(y'[x]+x y''[x]==0)
ode[[1094]]=(a y[x]+y'[x]+x y''[x]==0)
ode[[1095]]=(l x y[x]+y'[x]+x y''[x]==0)
ode[[1096]]=((a+x) y[x]+y'[x]+x y''[x]==0)
ode[[1097]]=(a y[x]-y'[x]+x y''[x]==0)
ode[[1098]]=(-a x^3 y[x]-y'[x]+x y''[x]==0)
ode[[1099]]=((E^x^3-v^2) x^3 y[x]-y'[x]+x y''[x]==0)
ode[[1100]]=(-E^x-x y[x]+2 y'[x]+x y''[x]==0)
ode[[1101]]=(a x y[x]+2 y'[x]+x y''[x]==0)
ode[[1102]]=(a x^2 y[x]+2 y'[x]+x y''[x]==0)
ode[[1103]]=(a y[x]-2 y'[x]+x y''[x]==0)
ode[[1104]]=(a y[x]+v y'[x]+x y''[x]==0)
ode[[1105]]=(b x y[x]+a y'[x]+x y''[x]==0)
ode[[1106]]=(b x^a1 y[x]+a y'[x]+x y''[x]==0)
ode[[1107]]=(a y[x]+(b+x) y'[x]+x y''[x]==0)
ode[[1108]]=(a y[x]+(a+b+x) y'[x]+x y''[x]==0)
ode[[1109]]=(-E^x x (1+x)-y[x]-x y'[x]+x y''[x]==0)
ode[[1110]]=(-a y[x]-x y'[x]+x y''[x]==0)
ode[[1111]]=(y[x]-(1+x) y'[x]+x y''[x]==0)
ode[[1112]]=(-2 (-1+x) y[x]-(1+x) y'[x]+x y''[x]==0)
ode[[1113]]=(-a y[x]+(b-x) y'[x]+x y''[x]==0)
ode[[1114]]=(-y[x]-2 (-1+x) y'[x]+x y''[x]==0)
ode[[1115]]=(-(-3+2 x) y[x]-(-2+3 x) y'[x]+x y''[x]==0)
ode[[1116]]=(a n y[x]+(b+n+a x) y'[x]+x y''[x]==0)
ode[[1117]]=(a b x y[x]-(a+b) (1+x) y'[x]+x y''[x]==0)
ode[[1118]]=((b m+a n+a b x) y[x]+(m+n+(a+b) x) y'[x]+x y''[x]==0)
ode[[1119]]=((2 a b+a^2 x) y[x]-2 (b+a x) y'[x]+x y''[x]==0)
ode[[1120]]=((d+c x) y[x]+(b+a x) y'[x]+x y''[x]==0)
ode[[1121]]=((-1+x) y[x]-(-x+x^2) y'[x]+x y''[x]==0)
ode[[1122]]=(-x (3+x) y[x]-(-2-x+x^2) y'[x]+x y''[x]==0)
ode[[1123]]=(b x^3 y[x]-(1+2 a x^2) y'[x]+x y''[x]==0)
ode[[1124]]=(2 n x y[x]-2 (-a+x^2) y'[x]+x y''[x]==0)
ode[[1125]]=(-4 x^5-4 x^3 y[x]+(-1+4 x^2) y'[x]+x y''[x]==0)
ode[[1126]]=((a+a^2 x^3) y[x]+(-1+2 a x^3) y'[x]+x y''[x]==0)
ode[[1127]]=((a+a Log[x]+a^2 x Log[x]^2) y[x]+(1+2 a x Log[x]) y'[x]+x y''[x]==0)
ode[[1128]]=(f[x] y[x]+(2+x f[x]) y'[x]+x y''[x]==0)
ode[[1129]]=((-6+3 x) y[x]-(-9+4 x) y'[x]+(-3+x) y''[x]==0)
ode[[1130]]=(a y[x]+y'[x]+2 x y''[x]==0)
ode[[1131]]=(a y[x]-(-1+x) y'[x]+2 x y''[x]==0)
ode[[1132]]=(a y[x]-(-1+2 x) y'[x]+2 x y''[x]==0)
ode[[1133]]=((-3+x) y[x]-(-4+3 x) y'[x]+(-1+2 x) y''[x]==0)
ode[[1134]]=(-(a+x) y[x]+4 x y''[x]==0)
ode[[1135]]=(-y[x]+2 y'[x]+4 x y''[x]==0)
ode[[1136]]=(-(2+x) y[x]+4 y'[x]+4 x y''[x]==0)
ode[[1137]]=(4 y[x]+l y[x]-(2+x) y[x]+4 x y''[x]==0)
ode[[1138]]=(-(-2 m-4 n+x) y[x]+4 m y'[x]+4 x y''[x]==0)
ode[[1139]]=(-(a+x) y[x]+8 y'[x]+16 x y''[x]==0)
ode[[1140]]=(c y[x]+b y'[x]+a x y''[x]==0)
ode[[1141]]=(3 b y[x]+(3 a+b x) y'[x]+a x y''[x]==0)
ode[[1142]]=(c (b+a x)^(1/5) y[x]+8 a y'[x]+5 (b+a x) y''[x]==0)
ode[[1143]]=(c y[x]+(a+b x) y'[x]+2 a x y''[x]==0)
ode[[1144]]=(c y[x]+(3 a+b x) y'[x]+2 a x y''[x]==0)
ode[[1145]]=((b0+a0 x) y[x]+(b1+a1 x) y'[x]+(b2+a2 x) y''[x]==0)
ode[[1146]]=(-6 y[x]+x^2 y''[x]==0)
ode[[1147]]=(-12 y[x]+x^2 y''[x]==0)
ode[[1148]]=(a y[x]+x^2 y''[x]==0)
ode[[1149]]=((b+a x) y[x]+x^2 y''[x]==0)
ode[[1150]]=((-2+x^2) y[x]+x^2 y''[x]==0)
ode[[1151]]=(-(2+a x^2) y[x]+x^2 y''[x]==0)
ode[[1152]]=((-6+a^2 x^2) y[x]+x^2 y''[x]==0)
ode[[1153]]=((-(-1+v) v+a x^2) y[x]+x^2 y''[x]==0)
ode[[1154]]=((c+b x+a x^2) y[x]+x^2 y''[x]==0)
ode[[1155]]=((-(-1+b) b+a x^k) y[x]+x^2 y''[x]==0)
ode[[1156]]=(-E^x x (2+x Log[x])+y[x]/Log[x]+x^2 y''[x]==0)
ode[[1157]]=(-x y[x]+a y'[x]+x^2 y''[x]==0)
ode[[1158]]=(-(a b+b^2 x^2) y[x]+a y'[x]+x^2 y''[x]==0)
ode[[1159]]=(-a x^2-y[x]+x y'[x]+x^2 y''[x]==0)
ode[[1160]]=(a y[x]+x y'[x]+x^2 y''[x]==0)
ode[[1161]]=(-(a+x) y[x]+x y'[x]+x^2 y''[x]==0)
ode[[1162]]=((-v^2+x^2) y[x]+x y'[x]+x^2 y''[x]==0)
ode[[1163]]=(-f[x]+(-v^2+x^2) y[x]+x y'[x]+x^2 y''[x]==0)
ode[[1164]]=((-v^2+l x^2) y[x]+x y'[x]+x^2 y''[x]==0)
ode[[1165]]=(-y[x]+(a+x) y'[x]+x^2 y''[x]==0)
ode[[1166]]=(-3 x^3+y[x]-x y'[x]+x^2 y''[x]==0)
ode[[1167]]=((b+a x^m) y[x]-x y'[x]+x^2 y''[x]==0)
ode[[1168]]=(2 x y'[x]+x^2 y''[x]==0)
ode[[1169]]=((-b^2+a x) y[x]+2 x y'[x]+x^2 y''[x]==0)
ode[[1170]]=((b+a x^2) y[x]+2 x y'[x]+x^2 y''[x]==0)
ode[[1171]]=((-n (1+n)+a x+l x^2) y[x]+2 x y'[x]+x^2 y''[x]==0)
ode[[1172]]=(a y[x]+2 (-1+x) y'[x]+x^2 y''[x]==0)
ode[[1173]]=(-(-1+b) b y[x]+2 (a+x) y'[x]+x^2 y''[x]==0)
ode[[1174]]=(-x^5 Log[x]+2 y[x]-2 x y'[x]+x^2 y''[x]==0)
ode[[1175]]=(-(4+12 a+a x^2) Cos[x]-x Sin[x]-4 y[x]-2 x y'[x]+x^2 y''[x]==0)
ode[[1176]]=((2+x^2) y[x]-2 x y'[x]+x^2 y''[x]==0)
ode[[1177]]=(-x^2 Sec[x]-2 x y'[x]+(2+x^2) y'[x]+x^2 y''[x]==0)
ode[[1178]]=(-x^3 Sec[x]+(2+x^2) y[x]-2 x y'[x]+x^2 y''[x]==0)
ode[[1179]]=((2+a^2 x^2) y[x]-2 x y'[x]+x^2 y''[x]==0)
ode[[1180]]=(-f[x]+(1-v^2+x^2) y[x]+3 x y'[x]+x^2 y''[x]==0)
ode[[1181]]=(y[x]+(-1+3 x) y'[x]+x^2 y''[x]==0)
ode[[1182]]=(-5 x+4 y[x]-3 x y'[x]+x^2 y''[x]==0)
ode[[1183]]=(-x^2 Log[x]-5 y[x]-3 x y'[x]+x^2 y''[x]==0)
ode[[1184]]=(x^2-x^4+6 y[x]-4 x y'[x]+x^2 y''[x]==0)
ode[[1185]]=(-(-4+2 x^3) y[x]+5 x y'[x]+x^2 y''[x]==0)
ode[[1186]]=(-x^3 Sin[x]+8 y[x]-5 x y'[x]+x^2 y''[x]==0)
ode[[1187]]=(b y[x]+a x y'[x]+x^2 y''[x]==0)
ode[[1188]]=(c y[x]+(b+a x) y'[x]+x^2 y''[x]==0)
ode[[1189]]=((c+b x^m) y[x]+a x y'[x]+x^2 y''[x]==0)
ode[[1190]]=((b+a x) y[x]+x^2 y'[x]+x^2 y''[x]==0)
ode[[1191]]=(-2 y[x]+x^2 y'[x]+x^2 y''[x]==0)
ode[[1192]]=(-y[x]+(-1+x^2) y'[x]+x^2 y''[x]==0)
ode[[1193]]=((-9+x) y[x]+x (1+x) y'[x]+x^2 y''[x]==0)
ode[[1194]]=((-1+3 x) y[x]+x (1+x) y'[x]+x^2 y''[x]==0)
ode[[1195]]=(-y[x]+x (3+x) y'[x]+x^2 y''[x]==0)
ode[[1196]]=((-1+x) y[x]-(-1+x) x y'[x]+x^2 y''[x]==0)
ode[[1197]]=(-(a+x) y[x]-(-2 x+x^2) y'[x]+x^2 y''[x]==0)
ode[[1198]]=(-(2+3 x) y[x]-(-2 x+x^2) y'[x]+x^2 y''[x]==0)
ode[[1199]]=(4 y[x]-x (4+x) y'[x]+x^2 y''[x]==0)
ode[[1200]]=(-(-1+v) v y[x]+2 x^2 y'[x]+x^2 y''[x]==0)
ode[[1201]]=(-4 y[x]+x (1+2 x) y'[x]+x^2 y''[x]==0)
ode[[1202]]=(2 (1+x) y[x]-2 x (1+x) y'[x]+x^2 y''[x]==0)
ode[[1203]]=(-2 y[x]+a x^2 y'[x]+x^2 y''[x]==0)
ode[[1204]]=((-2+b (a+b) x^2) y[x]+(a+2 b) x^2 y'[x]+x^2 y''[x]==0)
ode[[1205]]=(f[x] y[x]+a x^2 y'[x]+x^2 y''[x]==0)
ode[[1206]]=((d+a b x+c x^2) y[x]+x (b+2 a x) y'[x]+x^2 y''[x]==0)
ode[[1207]]=((c1+b1 x+a1 x^2) y[x]+x (b+a x) y'[x]+x^2 y''[x]==0)
ode[[1208]]=((-2+x^2) y[x]+x^3 y'[x]+x^2 y''[x]==0)
ode[[1209]]=((-2+x^2) y[x]+x (2+x^2) y'[x]+x^2 y''[x]==0)
ode[[1210]]=(((-1+(-1)^n) a+2 n x^2) y[x]-2 x (-a+x^2) y'[x]+x^2 y''[x]==0)
ode[[1211]]=((1+2 x^2+4 x^4) y[x]+4 x^3 y'[x]+x^2 y''[x]==0)
ode[[1212]]=(f[x] y[x]+x (b+a x^2) y'[x]+x^2 y''[x]==0)
ode[[1213]]=(-y[x]+x (1+x^3) y'[x]+x^2 y''[x]==0)
ode[[1214]]=(((-1)^n a-a^2+(1+2 a+2 n) x^2-x^4) y[x]+x^2 y''[x]==0)
ode[[1215]]=((c1+b1 x^n+a1 x^(2 n)) y[x]+x (b+a x^n) y'[x]+x^2 y''[x]==0)
ode[[1216]]=((DD+B x^a1+A x^(2 a1)+C x^b1) y[x]+x (b+a x^a1) y'[x]+x^2 y''[x]==0)
ode[[1217]]=(-(a+x Tan[x]) y[x]-(-x+2 x^2 Tan[x]) y'[x]+x^2 y''[x]==0)
ode[[1218]]=((a+x Cot[x]) y[x]+(x+2 x^2 Cot[x]) y'[x]+x^2 y''[x]==0)
ode[[1219]]=(y[x] (c+b x+a x^2-f[x]+f[x]^2+x f'[x])+2 x f[x] y'[x]+x^2 y''[x]==0)
ode[[1220]]=(y[x] (-(-1+v) v+x^2 (a+f[x]^2+f'[x]))+2 x^2 f[x] y'[x]+x^2 y''[x]==0)
ode[[1221]]=(y[x] (-v^2-x f[x]+x^2 (1+f[x]^2-f'[x]))+(x-2 x^2 f[x]) y'[x]+x^2 y''[x]==0)
ode[[1222]]=(2 y[x]+x y'[x]+(1+x^2) y''[x]==0)
ode[[1223]]=(-9 y[x]+x y'[x]+(1+x^2) y''[x]==0)
ode[[1224]]=(a y[x]+x y'[x]+(1+x^2) y''[x]==0)
ode[[1225]]=(y[x]-x y'[x]+(1+x^2) y''[x]==0)
ode[[1226]]=(-(-1+v) v y[x]+2 x y'[x]+(1+x^2) y''[x]==0)
ode[[1227]]=(2 y[x]-2 x y'[x]+(1+x^2) y''[x]==0)
ode[[1228]]=(a y[x]+3 x y'[x]+(1+x^2) y''[x]==0)
ode[[1229]]=(2 x-2 Cos[x]+2 y[x]+4 x y'[x]+(1+x^2) y''[x]==0)
ode[[1230]]=((-2+a) y[x]+a x y'[x]+(1+x^2) y''[x]==0)
ode[[1231]]=(-v (1+v) y[x]+(-1+x^2) y''[x]==0)
ode[[1232]]=((-n LegendreP[-1+n,x]+n x LegendreP[n,x])/(-1+x^2)-n (1+n) y[x]+(-1+x^2) y''[x]==0)
ode[[1233]]=((-n LegendreQ[-1+n,x]+n x LegendreQ[n,x])/(-1+x^2)-n (1+n) y[x]+(-1+x^2) y''[x]==0)
ode[[1234]]=(2+x y'[x]+(-1+x^2) y''[x]==0)
ode[[1235]]=(a y[x]+x y'[x]+(-1+x^2) y''[x]==0)
ode[[1236]]=(f[x] y[x]+x y'[x]+(-1+x^2) y''[x]==0)
ode[[1237]]=(2 x y'[x]+(-1+x^2) y''[x]==0)
ode[[1238]]=(-a+2 x y'[x]+(-1+x^2) y''[x]==0)
ode[[1239]]=(-l y[x]+2 x y'[x]+(-1+x^2) y''[x]==0)
ode[[1240]]=(-v (1+v) y[x]+2 x y'[x]+(-1+x^2) y''[x]==0)
ode[[1241]]=(-(-1+v) (2+v) y[x]-2 x y'[x]+(-1+x^2) y''[x]==0)
ode[[1242]]=(-(-x+x^2) y[x]-(1+3 x) y'[x]+(-1+x^2) y''[x]==0)
ode[[1243]]=((1+x^2) y[x]+4 x y'[x]+(-1+x^2) y''[x]==0)
ode[[1244]]=(-(-n+v) (1+n+v) y[x]+2 (1+n) x y'[x]+(-1+x^2) y''[x]==0)
ode[[1245]]=(-(1-n+v) (n+v) y[x]-2 (-1+n) x y'[x]+(-1+x^2) y''[x]==0)
ode[[1246]]=(-2 v y[x]-2 (-1+v) x y'[x]+(-1+x^2) y''[x]==0)
ode[[1247]]=((-1+a) a y[x]+2 a x y'[x]+(-1+x^2) y''[x]==0)
ode[[1248]]=((d+c x+b x^2) y[x]+a x y'[x]+(-1+x^2) y''[x]==0)
ode[[1249]]=(c y[x]+(b+a x) y'[x]+(-1+x^2) y''[x]==0)
ode[[1250]]=(12 y[x]+8 x y'[x]+(-a^2+x^2) y''[x]==0)
ode[[1251]]=(y[x]-(-1+x) y'[x]+x (1+x) y''[x]==0)
ode[[1252]]=(c y[x]+(b+a x) y'[x]+x (1+x) y''[x]==0)
ode[[1253]]=(y[x]+(2+3 x) y'[x]+x (1+x) y''[x]==0)
ode[[1254]]=(-(7 x+6 x^2) y[x]+(-x+x^2) y'[x]+(-2+x+x^2) y''[x]==0)
ode[[1255]]=(-2 y[x]+a y'[x]+(-1+x) x y''[x]==0)
ode[[1256]]=(-v (1+v) y[x]+(-1+2 x) y'[x]+(-1+x) x y''[x]==0)
ode[[1257]]=((b+(1+a) x) y'[x]+(-1+x) x y''[x]==0)
ode[[1258]]=(c y[x]+(b+a x) y'[x]+(-1+x) x y''[x]==0)
ode[[1259]]=(-l y[x]+(b+(1+a) x) y'[x]+(-1+x) x y''[x]==0)
ode[[1260]]=(a1 b1 d1+(-d1+(1+a1+b1) x) y'[x]+(-1+x) x y''[x]==0)
ode[[1261]]=((m+2 l p+2 l (-1-n+p) x) y[x]+2 (1+n+(1-2 l+n) x-l x^2) y'[x]+x (2+x) y''[x]==0)
ode[[1262]]=(-(2+x) y[x]+(-1+x+x^2) y'[x]+(1+x)^2 y''[x]==0)
ode[[1263]]=(-(30+20 x) (3 x+x^2)^(7/3)+y[x]+(-1+3 x) y'[x]+x (3+x) y''[x]==0)
ode[[1264]]=(-(3+2 x) y[x]+(1+x+x^2) y'[x]+(4+3 x+x^2) y''[x]==0)
ode[[1265]]=(y[x]-(-3+2 x) y'[x]+(-2+x) (-1+x) y''[x]==0)
ode[[1266]]=(-3 y[x]-(-2+x) y'[x]+(-2+x)^2 y''[x]==0)
ode[[1267]]=(-(-1+4 x) y[x]-(l-5 x+2 x^2) y'[x]+2 x^2 y''[x]==0)
ode[[1268]]=((b+a x) y[x]+(-1+2 x) y'[x]+2 (-1+x) x y''[x]==0)
ode[[1269]]=((1+v) y[x]+(-3-2 v+(5+2 v) x) y'[x]+2 (-1+x) x y''[x]==0)
ode[[1270]]=((8+17 x+12 x^2) y[x]+(8+21 x+10 x^2) y'[x]+(4+6 x+2 x^2) y''[x]==0)
ode[[1271]]=(y[x]+4 x^2 y''[x]==0)
ode[[1272]]=((1+4 a^2 x^2) y[x]+4 x^2 y''[x]==0)
ode[[1273]]=(-(-1+4 m^2-4 k x+x^2) y[x]+4 x^2 y''[x]==0)
ode[[1274]]=((-v^2+x) y[x]+4 x y'[x]+4 x^2 y''[x]==0)
ode[[1275]]=((1-m^2+2 (1+2 l-m) x-x^2) y[x]+4 x y'[x]+4 x^2 y''[x]==0)
ode[[1276]]=(-4 E^x Sqrt[x^3]-(1+4 x^2) y[x]+4 x y'[x]+4 x^2 y''[x]==0)
ode[[1277]]=(-(1+a x^2) y[x]+4 x y'[x]+4 x^2 y''[x]==0)
ode[[1278]]=(f[x] y[x]+4 x y'[x]+4 x^2 y''[x]==0)
ode[[1279]]=(-Log[x]-y[x]+5 x y'[x]+4 x^2 y''[x]==0)
ode[[1280]]=(-(3+12 x+4 x^2) y[x]+8 x y'[x]+4 x^2 y''[x]==0)
ode[[1281]]=((-1-4 x+4 x^2) y[x]-4 x (-1+2 x) y'[x]+4 x^2 y''[x]==0)
ode[[1282]]=((-4+x^2) (6+x^2) y[x]+4 x^3 y'[x]+4 x^2 y''[x]==0)
ode[[1283]]=(-4 x^2 Sqrt[E^x x^-x]+(-8+2 x+x^2 Log[x]^2) y[x]+4 x^2 Log[x] y'[x]+4 x^2 y''[x]==0)
ode[[1284]]=(-1-3 x-12 y[x]-2 (1+2 x) y'[x]+(1+2 x)^2 y''[x]==0)
ode[[1285]]=((-1+a) a y[x]+(-a+(2+4 a) x) y'[x]+x (-1+4 x) y''[x]==0)
ode[[1286]]=(-Log[-1+3 x]^2-9 y[x]+3 (-1+3 x) y'[x]+(-1+3 x)^2 y''[x]==0)
ode[[1287]]=(-20 y[x]+3 (-1+2 x) y'[x]+9 (-1+x) x y''[x]==0)
ode[[1288]]=((3+4 x) y[x]+16 x^2 y''[x]==0)
ode[[1289]]=(-(5+4 x) y[x]+32 x y'[x]+16 x^2 y''[x]==0)
ode[[1290]]=(-3 y[x]+27 x y'[x]+(4+27 x^2) y''[x]==0)
ode[[1291]]=(53 y[x]+(-40+152 x) y'[x]+48 (-1+x) x y''[x]==0)
ode[[1292]]=(-2 y[x]+25 (-1+2 x) y'[x]+50 (-1+x) x y''[x]==0)
ode[[1293]]=(y[x]+(-48+120 x) y'[x]+144 (-1+x) x y''[x]==0)
ode[[1294]]=(y[x]+(-96+168 x) y'[x]+144 (-1+x) x y''[x]==0)
ode[[1295]]=((f+d x+c x^2) y[x]+b x y'[x]+a x^2 y''[x]==0)
ode[[1296]]=((c0+b0 x+a0 x^2) y[x]+(b1 x+a1 x^2) y'[x]+a2 x^2 y''[x]==0)
ode[[1297]]=(b y[x]+a x y'[x]+(1+a x^2) y''[x]==0)
ode[[1298]]=(c y[x]+b x y'[x]+(1+a x^2) y''[x]==0)
ode[[1299]]=(2 a^2 x y'[x]+(-1+a^2 x^2) y''[x]==0)
ode[[1300]]=(-2 a^2 y[x]+2 a^2 x y'[x]+(-1+a^2 x^2) y''[x]==0)
ode[[1301]]=(-2 a y[x]+2 b y'[x]+(b x+a x^2) y''[x]==0)
ode[[1302]]=(A0 (b+a x) y[x]+A1 (b+a x) y'[x]+A2 (b+a x)^2 y''[x]==0)
ode[[1303]]=(g y[x]+(f+d x) y'[x]+(c+b x+a x^2) y''[x]==0)
ode[[1304]]=(-(3+2 x) y[x]+x y'[x]+x^3 y''[x]==0)
ode[[1305]]=(-y[x]+2 x y'[x]+x^3 y''[x]==0)
ode[[1306]]=((a+b x+a x^2) y[x]+x^2 y'[x]+x^3 y''[x]==0)
ode[[1307]]=(-2 y[x]+x (1+x) y'[x]+x^3 y''[x]==0)
ode[[1308]]=(-Log[x]^3+x y[x]-x^2 y'[x]+x^3 y''[x]==0)
ode[[1309]]=(x y[x]-(-1+x^2) y'[x]+x^3 y''[x]==0)
ode[[1310]]=(-1+x y[x]+3 x^2 y'[x]+x^3 y''[x]==0)
ode[[1311]]=(-v (1+v) x y[x]+(1+2 x^2) y'[x]+x (1+x^2) y''[x]==0)
ode[[1312]]=(-2 x y[x]+2 (-1+x^2) y'[x]+x (1+x^2) y''[x]==0)
ode[[1313]]=(-(-n+v) (1+n+v) x y[x]+(1+2 n+2 (1+n) x^2) y'[x]+x (1+x^2) y''[x]==0)
ode[[1314]]=((-1+n-v) (n+v) x y[x]-(-1+2 n+2 (-1+n) x^2) y'[x]+x (1+x^2) y''[x]==0)
ode[[1315]]=(a x^3 y[x]+y'[x]+x (-1+x^2) y''[x]==0)
ode[[1316]]=(-x y[x]+(-1+x^2) y'[x]+x (-1+x^2) y''[x]==0)
ode[[1317]]=(x y[x]+(-1+3 x^2) y'[x]+x (-1+x^2) y''[x]==0)
ode[[1318]]=(c x y[x]+(b+a x^2) y'[x]+x (-1+x^2) y''[x]==0)
ode[[1319]]=(-6 x y[x]-y'[x]+x (2+x^2) y''[x]==0)
ode[[1320]]=((2+4 x+x^2) y[x]-(-2-2 x+3 x^2+x^3) y'[x]+x (-2+x^2) y''[x]==0)
ode[[1321]]=((1+2 x) y[x]-x (1+2 x) y'[x]+x^2 (1+x) y''[x]==0)
ode[[1322]]=(2x(2+3 x) y'[x]+x^2 (1+x) y''[x]==0)
ode[[1323]]=(y''[x]==(2 (1+x) y[x])/((-1+x) x)-(2 (-2+x) y'[x])/((-1+x) x))
ode[[1324]]=(y''[x]==-(((-6+9 x) y[x])/((-1+x) x^2))+((-4+5 x) y'[x])/((-1+x) x))
ode[[1325]]=(y''[x]==-(((-alpha beta+a b x) y[x])/((-1+x) x^2))-((-1+alpha+beta+(1+a+b) x) y'[x])/((-1+x) x))
ode[[1326]]=(y''[x]==-(y[x]/(x (1+x)^2))-y'[x]/(1+x))
ode[[1327]]=(y''[x]==-(y[x]/((-2+x) x^2))+(2 y'[x])/((-2+x) x))
ode[[1328]]=(y''[x]==(2 y[x])/((-1+x)^2 x))
ode[[1329]]=(y''[x]==-(((-q+alpha beta x) y[x])/((-1+x) x (-a+x)))-((a gamma1-(1+alpha+beta-delta+a (delta+gamma1)) x+(1+alpha+beta) x^2) y'[x])/((-1+x) x (-a+x)))
ode[[1330]]=(y''[x]==-(((E+DD x) y[x])/((-a+x) (-b+x) (-c+x)))-((C+B x+A x^2) y'[x])/((-a+x) (-b+x) (-c+x)))
ode[[1331]]=(y''[x]==-(((-3+x) y[x])/(2 (-2+x) x^2))+((-4+x) y'[x])/(2 (-2+x) x))
ode[[1332]]=(y''[x]==-(((1+3 x) y[x])/(4 x^2 (1+x)))+y'[x]/(1+x))
ode[[1333]]=(y''[x]==(v (1+v) y[x])/(4 x^2)-((-1+3 x) y'[x])/(2 (-1+x) x))
ode[[1334]]=(y''[x]==-(((c^2+(a^2-b^2) x) y[x])/(4 (-1+x) x^2))-((-1+(1+a) x) y'[x])/((-1+x) x))
ode[[1335]]=(y''[x]==-(((b+a x) y[x])/(4 (-1+x)^2 x))-((-1+3 x) y'[x])/(2 (-1+x) x))
ode[[1336]]=(y''[x]==-(((1-3 x) y[x])/((-1+x) (-1+2 x)^2)))
ode[[1337]]=(y''[x]==-(((a-b) y[x])/(4 (a+x)^2 (b+x)))-((a+2 b+3 x) y'[x])/(2 (a+x) (b+x)))
ode[[1338]]=(y''[x]==y[x]/(3 (-2+x) x^2)+((-1+6 x) y'[x])/(3 (-2+x) x))
ode[[1339]]=(y''[x]==-(((-c d+a b x) y[x])/(x^2 (1+a x)))-(((1+c-d) x+a (2+b) x^2) y'[x])/(x^2 (1+a x)))
ode[[1340]]=(y''[x]==-(((6 b+2 a x) y[x])/(x^2 (b+a x)))+(2 (2 b+a x) y'[x])/(x (b+a x)))
ode[[1341]]=(y''[x]==A x-((-b+a v x) y[x])/(x^2 (b+a x))-((b+2 a x) y'[x])/(x (b+a x)))
ode[[1342]]=(y''[x]==-((a y[x])/x^4))
ode[[1343]]=(y''[x]==-((((1-a) a x^2-b (b+x)) y[x])/x^4))
ode[[1344]]=(y''[x]==-(((E^(2/x)-v^2) y[x])/x^4))
ode[[1345]]=(y''[x]==(2 y[x])/x^4-y'[x]/x^3)
ode[[1346]]=(y''[x]==-(((a b+(a+b) x) y[x])/x^4)+((a+b) y'[x])/x^2)
ode[[1347]]=(y''[x]==-(y[x]/x^4)-y'[x]/x)
ode[[1348]]=(y''[x]==-(((b x^2+a (1+x^4)) y[x])/x^4)-y'[x]/x)
ode[[1349]]=(y''[x]==-(y[x]/x^4)-((1+x^2) y'[x])/x^3)
ode[[1350]]=(y''[x]==-((a^2 y[x])/x^4)-(2 y'[x])/x)
ode[[1351]]=(y''[x]==y[x]/x^4-((1+2 x^2) y'[x])/x^3)
ode[[1352]]=(y''[x]==-((b y[x])/x^4)-(2 (a+x) y'[x])/x^2)
ode[[1353]]=(y''[x]==-(y[x]/x^4)+((-1+2 x^2) y'[x])/x^3)
ode[[1354]]=(y''[x]==-((2 y[x])/x^4)+((-1+2 x^2) y'[x])/x^3)
ode[[1355]]=(y''[x]==(x y[x])/(1+x^3)-((-1+x^3) y'[x])/(x (1+x^3)))
ode[[1356]]=(y''[x]==-(((-n^2-v (1+v) x^2) y[x])/(x^2 (1+x^2)))-((1+2 x^2) y'[x])/(x (1+x^2)))
ode[[1357]]=(y''[x]==-(((c+b x^2) y[x])/(x^2 (1+x^2)))-((-1+a+a x^2) y'[x])/(x (1+x^2)))
ode[[1358]]=(y''[x]==-(((-2+x^2) y[x])/(x^2 (-1+x^2)))+((-2+x^2) y'[x])/(x (-1+x^2)))
ode[[1359]]=(y''[x]==-((v (1+v) y[x])/(x^2 (-1+x^2)))-(2 x y'[x])/(-1+x^2))
ode[[1360]]=(y''[x]==(v (1+v) y[x])/x^2-(2 x y'[x])/(-1+x^2))
ode[[1361]]=(y''[x]==-(((a (1+a)-a (3+a) x^2) y[x])/(x^2 (-1+x^2)))+(2 x y'[x])/(-1+x^2))
ode[[1362]]=(y''[x]==-(((2 a x^2+n (1+n) (-1+x^2)+(a-n) (1+a+n) x^2 (-1+x^2)) y[x])/(x^2 (-1+x^2)))+(2 x y'[x])/(-1+x^2))
ode[[1363]]=(y''[x]==-((b y[x])/x^2)-((-2+a+a x^2) y'[x])/(x (-1+x^2)))
ode[[1364]]=(y''[x]==-(((-a (1+a)+((-1+a) a-v (1+v)) x^2-b (1+2 a-c) c x^c+b (-1+2 a-c) c x^(2+c)+b^2 c^2 x^(2 c) (-1+x^2)) y[x])/(x^2 (-1+x^2)))+((-2 a+2 (-1+a) x^2+2 b c x^c (-1+x^2)) y'[x])/(x (-1+x^2)))
ode[[1365]]=(y''[x]==-((a y[x])/(1+x^2)^2))
ode[[1366]]=(y''[x]==-(y[x]/(1+x^2)^2)-(2 x y'[x])/(1+x^2))
ode[[1367]]=(y''[x]==-(((m^2-n (1+n) (1+x^2)+a^2 (1+x^2)^2) y[x])/(1+x^2)^2)-(2 x y'[x])/(1+x^2))
ode[[1368]]=(y''[x]==-((b y[x])/(1+x^2)^2)-(a x y'[x])/(1+x^2))
ode[[1369]]=(y''[x]==-((a y[x])/(-1+x^2)^2))
ode[[1370]]=(y''[x]==(a^2 y[x])/(-1+x^2)^2-(2 x y'[x])/(-1+x^2))
ode[[1371]]=(y''[x]==-(((-a^2-lambda (-1+x^2)) y[x])/(-1+x^2)^2)-(2 x y'[x])/(-1+x^2))
ode[[1372]]=(y''[x]==-(((-k^2+(-1+x^2) (c+b x+a x^2)) y[x])/(-1+x^2)^2)-(2 x y'[x])/(-1+x^2))
ode[[1373]]=(y''[x]==-(((-m^2-n (1+n) (-1+x^2)-a^2 (-1+x^2)^2) y[x])/(-1+x^2)^2)-(2 x y'[x])/(-1+x^2))
ode[[1374]]=(y''[x]==-(((2 a+v (1+v)+(2 a (-1+2 a)-v (1+v)) x^2) y[x])/(-1+x^2)^2)+(2 (-1+2 a) x y'[x])/(-1+x^2))
ode[[1375]]=(y''[x]==-(((4 a (a-n) x^2-(2 a+(-n+v) (1+n+v)) (-1+x^2)) y[x])/(-1+x^2)^2)-(2 (1-2 a+n) x y'[x])/(-1+x^2))
ode[[1376]]=(y''[x]==-((b y[x])/(x^2 (a+x^2)))-((a+2 x^2) y'[x])/(x (a+x^2)))
ode[[1377]]=(y''[x]==-((b^2 y[x])/(a^2+x^2)^2))
ode[[1378]]=(y''[x]==-(((2+2 x-2 x^2) y[x])/((-1+x)^2 x^2))-(2 (-1+x^2) y'[x])/((-1+x)^2 x))
ode[[1379]]=(y''[x]==(12 y[x])/((1+x)^2 (3+2 x+x^2)))
ode[[1380]]=(y''[x]==-((b y[x])/(x^2 (-a+x)^2)))
ode[[1381]]=(y''[x]==c-(b y[x])/(x^2 (-a+x)^2))
ode[[1382]]=(y''[x]==(c y[x])/((-a+x)^2 (-b+x)^2))
ode[[1383]]=(y''[x]==-((alpha (a-b)^2 beta y[x])/((-a+x)^2 (-b+x)^2))-(((1+alpha+beta) (-a+x)^2 (-b+x)+(1-alpha-beta) (-a+x) (-b+x)^2) y'[x])/((-a+x)^2 (-b+x)^2))
ode[[1384]]=(y''[x]==-(((-b^2+2 (3+a) b x-(-1+a^2) x^2) y[x])/(4 x^2)))
ode[[1385]]=(y''[x]==-(((-3+a+a x^2) y[x])/(4 (1+x^2)^2)))
ode[[1386]]=(y''[x]==(18 y[x])/((1+2 x)^2 (1+x+x^2)))
ode[[1387]]=(y''[x]==(3 y[x])/(4 (1+x+x^2)^2))
ode[[1388]]=(y''[x]==-(((v (1+v) (-1+x)-a^2 x) y[x])/(4 (-1+x)^2 x^2))-((-1+3 x) y'[x])/(2 (-1+x) x))
ode[[1389]]=(y''[x]==-(((-v (1+v) (-1+x)^2-4 n^2 x) y[x])/(4 (-1+x)^2 x^2))-((-1+3 x) y'[x])/(2 (-1+x) x))
ode[[1390]]=(y''[x]==-((3 y[x])/(16 (-1+x)^2 x^2)))
ode[[1391]]=(y''[x]==-(((5+15 a x^2) y[x])/(x^2 (1+a x^2)))+((5+7 a x^2) y'[x])/(x (1+a x^2)))
ode[[1392]]=(y''[x]==-(((e+d x+c x^2) y[x])/(a (-1+x^2)^2))-(b x y'[x])/(a (-1+x^2)))
ode[[1393]]=(y''[x]==-(((d+c x+b x^2) y[x])/(a (-1+x)^2 x^2)))
ode[[1394]]=(y''[x]==-((c y[x])/(x^2 (b+a x)^2))-(2 y'[x])/x)
ode[[1395]]=(y''[x]==-(y[x]/(b+a x)^4))
ode[[1396]]=(y''[x]==-((A y[x])/(c+b x+a x^2)^2))
ode[[1397]]=(y''[x]==y[x]/x^5-y'[x]/x^4)
ode[[1398]]=(y''[x]==-(((-1-(1+2 v)^2+x^2) y[x])/(-1+x^2)^2)-((-1+3 x^2) y'[x])/(x (-1+x^2)))
ode[[1399]]=(y''[x]==-((36 (1+x)^2 y[x])/((-1+x)^2 (5+3 x)^2))+((1+3 x) y'[x])/((-1+x) (1+x)))
ode[[1400]]=(y''[x]==-((a y[x])/x^6)+y'[x]/x)
ode[[1401]]=(y''[x]==-((b y[x])/x^6)-((a+3 x^2) y'[x])/x^3)
ode[[1402]]=(y''[x]==-(((4 a (1+a) x^4-2 a x^2 (-1+x^2)+(-1+x^2)^2 (-v^2+x^2)) y[x])/(x^2 (-1+x^2)^2))-((-1+(1-4 a) x^2) y'[x])/(x (-1+x^2)))
ode[[1403]]=(y''[x]==-((((a1 b1 (c1-c2) (c1-c3))/(-c1+x)+(a2 b2 (-c1+c2) (c2-c3))/(-c2+x)+(a3 b3 (-c1+c3) (-c2+c3))/(-c3+x)) y[x])/((-c1+x) (-c2+x) (-c3+x)))-((1-a1-b1)/(-c1+x)+(1-a2-b2)/(-c2+x)+(1-a3-b3)/(-c3+x)) y'[x])
ode[[1404]]=(y''[x]==-(((1-2 x^2) y[x])/(4 x^6))-((1+2 x^2) y'[x])/x^3)
ode[[1405]]=(y''[x]==-(((1+10 x^2+a x^4) y[x])/(4 x^6))+((1+2 x^2) y'[x])/x^3)
ode[[1406]]=(y''[x]==-((27 x y[x])/(16 (-1+x^3)^2)))
ode[[1407]]=(y''[x]==-((((al1 (-a2 b1+a1 b2) (a3 b1-a1 b3) bl1)/(-a1+b1 x)+(al2 (-a2 b1+a1 b2) (-a3 b2+a2 b3) bl2)/(-a2+b2 x)+(al3 (a3 b1-a1 b3) (-a3 b2+a2 b3) bl3)/(-a3+b3 x)) y[x])/((-a1+b1 x) (-a2+b2 x) (-a3+b3 x)))-((b1 (1-al1-bl1))/(-a1+b1 x)+(b2 (1-al2-bl2))/(-a2+b2 x)+(b3 (1-al3-bl3))/(-a3+b3 x)) y'[x])
ode[[1408]]=(y''[x]==-(((B+A x^2) y[x])/(x (-a1+x^2) (-a2+x^2) (-a3+x^2)))-((-(-a1+x^2) (-a2+x^2) (-a3+x^2)+x^2 ((-a1+x^2) (-a2+x^2)+(-a1+x^2) (-a3+x^2)+(-a2+x^2) (-a3+x^2))) y'[x])/(x (-a1+x^2) (-a2+x^2) (-a3+x^2)))
ode[[1409]]=(y''[x]==-b^2 x^(-2 a) y[x]-(a y'[x])/x)
ode[[1410]]=(y''[x]==-(((s+a r x^b) y[x])/(x^2 (-1+a x^b)))-((q+a p x^b) y'[x])/(x (-1+a x^b)))
ode[[1411]]=(y''[x]== y[x]/(1+E^x))
ode[[1412]]=(y''[x]== Log[x]^2 y[x]+y'[x]/(x Log[x]))
ode[[1413]]=(y''[x]==-(y[x]/(x^2 (-1+Log[x])))+y'[x]/(x (-1+Log[x])))
ode[[1414]]=(y''[x]==-Csch[x]^2 (-(-1+n) n-a^2 Sinh[x]^2) y[x])
ode[[1415]]=(y''[x]==-(-a^2+n^2) y[x]-2 n Coth[x] y'[x])
ode[[1416]]=(y''[x]==-(-n+v) (1+n+v) y[x]-(1+2 n) Cot[x] y'[x])
ode[[1417]]=(y''[x]==-Sin[x]^2 y[x]-Csc[x] (-Cos[x]+Sin[x]^2) y'[x])
ode[[1418]]=(y''[x]==(Sin[x] y[x])/(x Cos[x]-Sin[x])-(x Sin[x] y'[x])/(x Cos[x]-Sin[x]))
ode[[1419]]=(y''[x]== -((Sec[x] (2 x Cos[x]-x Sin[x]) y[x])/x^2)-(Sec[x] (-2 x Cos[x]+x^2 Sin[x]) y'[x])/x^2)
ode[[1420]]=(-((-1+n) n+a Cos[x]^2) y[x]+Cos[x]^2 y''[x]==0)
ode[[1421]]=(y''[x]==-a^2 n Sec[a x]^2 (Cos[a x]^2+(-1+n) Sin[a x]^2) y[x]-a (-1+n) Sec[a x]^2 Sin[2 a x] y'[x])
ode[[1422]]=(y''[x]==2 Csc[x]^2 y[x])
ode[[1423]]=(y''[x]==-a Csc[x]^2 y[x])
ode[[1424]]=(-((-1+n) n+a Sin[x]^2) y[x]+Sin[x]^2 y''[x]==0)
ode[[1425]]=(y''[x]==-(-3+3 a-(3-2 a) Cos[x]-a^2 Cos[x]^2) Csc[x]^2 y[x])
ode[[1426]]=(-(2+3 a+b^2/(-3+2 a)^2+b Cos[x]+a^2 Cos[x]^2) y[x]+Sin[x]^2 y''[x]==0)
ode[[1427]]=(y''[x]==-Csc[x]^2 (-(-1+a) a-(-(1+a)^2+a^2 b^2) Sin[x]^2-a (1+a) b Sin[2 x]) y[x])
ode[[1428]]=(y''[x]==-Csc[x]^2 (c+a Cos[x]^2+b Sin[x]^2) y[x])
ode[[1429]]=(y''[x]==Csc[x]^2 y[x]-Cot[x] y'[x])
ode[[1430]]=(y''[x]==-Csc[x]^2 (-n^2+v (1+v) Sin[x]^2) y[x]-Cot[x] y'[x])
ode[[1431]]=(y''[x]==-2 y[x]+Cot[2 x] y'[x])
ode[[1432]]=(y''[x]==-(1/4) Csc[x]^2 (-1-17 Sin[x]^2) y[x]-Cot[x] y'[x])
ode[[1433]]=(y''[x]==Sqrt[Cos[x]]-(Sec[x]^2 (2 x^2-24 Cos[x]^2+x^2 Sin[x]^2) y[x])/(4 x^2)-Tan[x] y'[x])
ode[[1434]]=(y''[x]==-(((e+d Cos[x]+c Cos[x]^2) Csc[x]^2 y[x])/a)-(b Cot[x] y'[x])/a)
ode[[1435]]=(y''[x]==-4 Csc[x]^3 Sin[3 x] y[x])
ode[[1436]]=(y''[x]==-(1/4) Csc[x]^2 (2-4 n^2-Cos[x]^2+4 v (1+v) Sin[x]^2) y[x])
ode[[1437]]=(y''[x]==Tan[x]^2 y[x]+Csc[x] Sec[x] (1+3 Sin[x]^2) y'[x])
ode[[1438]]=(y''[x]==-Csc[x]^2 Sec[x]^2 (-(-1+n) n Cos[x]^2-(-1+m) m Sin[x]^2-a Cos[x]^2 Sin[x]^2) y[x])
ode[[1439]]=(y''[x]==(phi'[x] y'[x])/(-phi[a]+phi[x])-(y[x] (-n (1+n) (-phi[a]+phi[x])^2+phi''[a]))/(-phi[a]+phi[x]))
ode[[1440]]=(y''[x]== -((y'[x] (phi[x^3]-phi[x] phi'[x]-phi''[x]))/(phi[x]^2+phi'[x]))-(y[x] (-phi[x]^2 phi'[x]+phi'[x]^2-phi[x] phi''[x]))/(phi[x]^2+phi'[x]))
ode[[1441]]=(y''[x]== -(1/(-JacobiSN[a,k]^2+JacobiSN[x,k]^2))-((2-4 (1+k^2) JacobiSN[a,k]^2+6 k^2 JacobiSN[a,k]^4) y[x])/(-JacobiSN[a,k]^2+JacobiSN[x,k]^2)-((-JacobiCN[x,k] JacobiDN[x,k]-2 JacobiSN[x,k]) y'[x])/(-JacobiSN[a,k]^2+JacobiSN[x,k]^2))
ode[[1442]]=(y''[x]== y[x]/f[x]-(x y'[x])/f[x])
ode[[1443]]=(y''[x]== -((g[x] y[x])/f[x])-(f'[x] y'[x])/(2 f[x]))
ode[[1444]]=(y''[x]== -b f[x]^(2 a) y[x]-(a f'[x] y'[x])/f[x])
ode[[1445]]=(y''[x]== -((y'[x] (2 f[x] g[x] g'[x]^2-(-1+g[x]^2) (2 f'[x] g'[x]+f[x] g''[x])))/(f[x] (-1+g[x]^2) g'[x]))-(y[x] (-f[x] g'[x]^2 (2 g[x] f'[x]+v (1+v) f[x] g'[x])+(-1+g[x]^2) (-f[x] g'[x] f''[x]+f'[x] (2 f'[x] g'[x]+f[x] g''[x]))))/(f[x]^2 (-1+g[x]^2) g'[x]))
ode[[1446]]=(y''[x]== -(((-1+x) y[x])/x^4)-y'[x]/x)
ode[[1447]]=(y''[x]== -(((-1-x) y[x])/x^4)-y'[x]/x)
ode[[1448]]=(y''[x]== -((b^2 y[x])/(-a^2+x^2)^2))
(*chapter 3*)
ode[[1449]]=(-lambda y[x]+y'''[x]==0)
ode[[1450]]=(-b x+a x^3 y[x]+y'''[x]==0)
ode[[1451]]=(-a x^b y[x]+y'''[x]==0)
ode[[1452]]=(-4 y[x]+3 y'[x]+y'''[x]==0)
ode[[1453]]=(-E^(2 a x) Sin[x]^2-a^2 y'[x]+y'''[x]==0)
ode[[1454]]=(a y[x]+2 a x y'[x]+y'''[x]==0)
ode[[1455]]=(-a b y[x]+(-1+a+b) x y'[x]-x^2 y''[x]+y'''[x]==0)
ode[[1456]]=((-1+c) x^(-3+2 c) y[x]+x^(-2+2 c) y'[x]+y'''[x]==0)
ode[[1457]]=(b y[x]-3 (a+2 WeierstrassP[x,{g2,g3}]) y'[x]+y'''[x]==0)
ode[[1458]]=(1/2 y[x] (-a+(1-n^2) D[WeierstrassP[x,{g2,g3}],x])+(1-n^2) WeierstrassP[x,{g2,g3}] y'[x]+y'''[x]==0)
ode[[1459]]=(-2 n (1+n) y[x] D[WeierstrassP[x,{g2,g3}],x]-(a+4 n (1+n) WeierstrassP[x,{g2,g3}]) y'[x]+y'''[x]==0)
ode[[1460]]=(B y[x] D[WeierstrassP[x,{g2,g3}],x]+(a+A WeierstrassP[x,{g2,g3}]) y'[x]+y'''[x]==0)
ode[[1461]]=((b-3 k^2 JacobiCN[z,x] JacobiDN[z,x] JacobiSN[z,x]+c JacobiSN[z,x]^2) y[x]-(a+3 k^2 JacobiSN[z,x]^2) y'[x]+y'''[x]==0)
ode[[1462]]=(b y[x]-(a+6 k^2 Sin[x]^2) y'[x]+y'''[x]==0)
ode[[1463]]=(y[x] f'[x]+2 f[x] y'[x]+y'''[x]==0)
ode[[1464]]=(10 y[x]-3 y'[x]-2 y''[x]+y'''[x]==0)
ode[[1465]]=(-Sinh[x]+2 a^2 y[x]-a^2 y'[x]-2 y''[x]+y'''[x]==0)
ode[[1466]]=(-E^(a x)-a^3 y[x]+3 a^2 y'[x]-3 a y''[x]+y'''[x]==0)
ode[[1467]]=(a0 y[x]+a1 y'[x]+a2 y''[x]+y'''[x]==0)
ode[[1468]]=(-8 a x y[x]+2 (-1+2 a+4 x^2) y'[x]-6 x y''[x]+y'''[x]==0)
ode[[1469]]=(a^3 x^3 y[x]+3 a^2 x^2 y'[x]+3 a x y''[x]+y'''[x]==0)
ode[[1470]]=(-Log[x]+Sin[x] y[x]-2 Cos[x] y'[x]-Sin[x] y''[x]+y'''[x]==0)
ode[[1471]]=(f[x] y[x]+y'[x]+f[x] y''[x]+y'''[x]==0)
ode[[1472]]=(f[x] (2 y[x]-2 x y'[x]+x^2 y''[x])+y'''[x]==0)
ode[[1473]]=(y[x] (f[x] g[x]+g'[x])+g[x] y'[x]+f[x] y''[x]+y'''[x]==0)
ode[[1474]]=(y[x] (4 f[x] g[x]+2 g'[x])+(2 f[x]^2+4 g[x]+f'[x]) y'[x]+3 f[x] y''[x]+y'''[x]==0)
ode[[1475]]=(18 E^x-3 y[x]-11 y'[x]-8 y''[x]+4 y'''[x]==0)
ode[[1476]]=(-2 n (3+n) (-3+4 n) y[x] phi'[x]-36 n^2 WeierstrassP[x,{g2,g3}] y'[x]+27 y'''[x]==0)
ode[[1477]]=(x y[x]+3 y''[x]+x y'''[x]==0)
ode[[1478]]=(-a x^2 y[x]+3 y''[x]+x y'''[x]==0)
ode[[1479]]=(-a y[x]-x y'[x]+(a+b) y''[x]+x y'''[x]==0)
ode[[1480]]=((-1+x) y[x]-(-1-2 v+x) y'[x]-(2 v+x) y''[x]+x y'''[x]==0)
ode[[1481]]=(-f[x]+2 y[x]+4 x y'[x]+(-3+x^2) y''[x]+x y'''[x]==0)
ode[[1482]]=(-b+a x y[x]+3 y''[x]+2 x y'''[x]==0)
ode[[1483]]=((1-2 nu) y[x]+(-5+6 nu+2 x) y'[x]-4 (-1+nu+x) y''[x]+2 x y'''[x]==0)
ode[[1484]]=((3 b k+2 c x) y[x]+6 (a k+b x) y'[x]+3 (k+2 a x) y''[x]+2 x y'''[x]==0)
ode[[1485]]=(2 y[x]-2 y'[x]-(-2+x) x y''[x]+(-2+x) x y'''[x]==0)
ode[[1486]]=(8 y[x]-8 x y'[x]+(-1+2 x) y'''[x]==0)
ode[[1487]]=(2 y'[x]+(4+x) y''[x]+(-1+2 x) y'''[x]==0)
ode[[1488]]=(a x^2 y[x]-6 y'[x]+x^2 y'''[x]==0)
ode[[1489]]=(-y[x]+(1+x) y''[x]+x^2 y'''[x]==0)
ode[[1490]]=((1+x^2) y'[x]-x y''[x]+x^2 y'''[x]==0)
ode[[1491]]=(x^2*y'''[x]+3*x*y''[x]+(4*a^2*x^(2*a)+1-4*nu^2*a^2)*y'[x]==4 a^3 x^(-1+2 a) y[x])
ode[[1492]]=(-2 n (1-2 m+2 x) y[x]+(m (-1+2 m)+4 (-m+n) x+2 x^2) y'[x]-3 x (-m+x) y''[x]+x^2 y'''[x]==0)
ode[[1493]]=(-f[x]+3 x y[x]+(2+x^2) y'[x]+4 x y''[x]+x^2 y'''[x]==0)
ode[[1494]]=(-Log[x]+4 y'[x]+5 x y''[x]+x^2 y'''[x]==0)
ode[[1495]]=(6 y'[x]+6 x y''[x]+x^2 y'''[x]==0)
ode[[1496]]=(a x^2 y[x]+6 y'[x]+6 x y''[x]+x^2 y'''[x]==0)
ode[[1497]]=(-x^2 y[x]+3 p (1+3 q) y'[x]-3 (p+q) x y''[x]+x^2 y'''[x]==0)
ode[[1498]]=(-2 a x y[x]+(6 n+a x^2) y'[x]-2 (1+n) x y''[x]+x^2 y'''[x]==0)
ode[[1499]]=((-(1/4)+nu^2-2 x+x^2) y[x]-(-(1/4)+nu^2+x^2) y'[x]-(-2 x+x^2) y''[x]+x^2 y'''[x]==0)
ode[[1500]]=(-nu (1+x) y[x]+nu (1+2 x) y'[x]-x (v+x) y''[x]+x^2 y'''[x]==0)
ode[[1501]]=((-(1/4)+nu^2) y[x]+(1/4-nu^2-2 x+x^2) y'[x]-2 (-x+x^2) y''[x]+x^2 y'''[x]==0)
ode[[1502]]=(2 x^2 y[x]-(-6+2 x^3) y'[x]-(-6 x+x^4) y''[x]+x^2 y'''[x]==0)
ode[[1503]]=(-3+1/x^2-2 Log[x]+10 y'[x]+8 x y''[x]+(1+x^2) y'''[x]==0)
ode[[1504]]=(-2 x y[x]+(2+x^2) y'[x]-2 x y''[x]+(2+x^2) y'''[x]==0)
ode[[1505]]=(a y[x]+(b+2 a x) y'[x]+3 (-1+2 x) y''[x]+2 (-1+x) x y'''[x]==0)
ode[[1506]]=(2 y[x]+4 (1+x) y'[x]+(-1+14 x+x^2) y''[x]+4 x^2 y'''[x]==0)
ode[[1507]]=(-f[x]+y[x]+x y'[x]+(beta+alpha x) y''[x]+x (b+a x) y'''[x]==0)
ode[[1508]]=((-1+nu^2+a x^3) y[x]+(1-nu^2) x y'[x]+x^3 y'''[x]==0)
ode[[1509]]=((-1+4 nu^2) y[x]+((1-4 nu^2) x+4 x^3) y'[x]+x^3 y'''[x]==0)
ode[[1510]]=((-1+nu^2+a (-1+nu) x^(2 nu)+b x^(3 nu)) y[x]+x (1-nu^2+a x^(2 nu)) y'[x]+x^3 y'''[x]==0)
ode[[1511]]=(x^3 (8+x)-6 (-1+x) x^3 Log[x]+2 y[x]-2 x y'[x]+3 x^2 y''[x]+x^3 y'''[x]==0)
ode[[1512]]=((1-a^2) x y'[x]+3 x^2 y''[x]+x^3 y'''[x]==0)
ode[[1513]]=(-2 (4+x^2) y[x]+x (8+x^2) y'[x]-4 x^2 y''[x]+x^3 y'''[x]==0)
ode[[1514]]=((-12+a x^3) y[x]+6 x^2 y''[x]+x^3 y'''[x]==0)
ode[[1515]]=((a (-a^2+4 c^2 nu^2)+4 b^2 c^2 (-a+c) x^(2 c)) y[x]+(1-4 c^2 nu^2+3 (-1+a) a x+4 b^2 c^2 x^(1+2 c)) y'[x]+3 (1-a) x^2 y''[x]+x^3 y'''[x]==0)
ode[[1516]]=((30+4 x) y[x]+5 (-6+x) x y'[x]+x^2 (3+x) y''[x]+x^3 y'''[x]==0)
ode[[1517]]=(-2 x^3+Log[x]-y[x]+2 x y'[x]+x^2 y''[x]+x^3 y'''[x]==0)
ode[[1518]]=(-12 y[x]+3 (1+2 x^2) y''[x]+x (1+x^2) y'''[x]==0)
ode[[1519]]=(-6 y[x]+6 (1+x) y'[x]-3 x (2+x) y''[x]+x^2 (3+x) y'''[x]==0)
ode[[1520]]=(-n (1+n) y[x]-2 (b+(-3+n+n^2) x) y'[x]+(3 a1 a2+3 a1 a3+3 a2 a3-6 (a1+a2+a3) x+9 x^2) y''[x]+2 (-a1+x) (-a2+x) (-a3+x) y'''[x]==0)
ode[[1521]]=(-4 (1+3 x) y[x]+x (4+10 x) y'[x]-x^2 (2+4 x) y''[x]+x^3 (1+x) y'''[x]==0)
ode[[1522]]=(-1+4 x^2 y'[x]-4 x^3 y''[x]+4 x^4 y'''[x]==0)
ode[[1523]]=(-4 (1+3 x^2) y[x]+x (4+10 x^2) y'[x]-x^2 (2+4 x^2) y''[x]+x^3 (1+x^2) y'''[x]==0)
ode[[1524]]=(-2 y[x]+x^2 y''[x]+x^6 y'''[x]==0)
ode[[1525]]=(a y[x]+6 x^5 y''[x]+x^6 y'''[x]==0)
ode[[1526]]=((1+6 x+8 x^2+4 x^3+x^4) y[x]+(-2-12 x-15 x^2-6 x^3+x^6) y'[x]-(-1-6 x-6 x^2+3 x^4+2 x^6) y''[x]+x^2 (1+2 x+2 x^2+x^4) y'''[x]==0)
ode[[1527]]=(-c y[x]+(-a+x)^3 (-b+x)^3 y'''[x]==0)
ode[[1528]]=(-Cos[x]-Sin[x] y'[x]+(1+2 Cos[x]) y''[x]+Sin[x] y'''[x]==0)
ode[[1529]]=(Sin[x]-Cos[x] y[x]-3 Sin[x] y'[x]+3 (1+Cos[x]) y''[x]+(x+Sin[x]) y'''[x]==0)
ode[[1530]]=(2 nu (1+nu) Sin[2 x] y[x]+(Cos[2 x]+4 nu (1+nu) Sin[x]^2) y'[x]+3 Cos[x] Sin[x] y''[x]+Sin[x]^2 y'''[x]==0)
ode[[1531]]=(y[x] h'[x]+h[x] y'[x]+g'[x] y'[x]+g[x] y''[x]+f'[x] y''[x]+A[x] (h[x] y[x]+g[x] y'[x]+f[x] y''[x])+f[x] y'''[x]==0)
ode[[1532]]=(n y[x]+x y'[x]+y'''[x]==0)
ode[[1533]]=(-n y[x]-x y'[x]+y'''[x]==0)
(*chapter 4*)
ode[[1534]]=y''''[x]==0
ode[[1535]]=(-f[x]+4 y[x]+y''''[x]==0)
ode[[1536]]=(lambda y[x]+y''''[x]==0)
ode[[1537]]=(-16 E^x^2 x^4+12 y[x]-12 y''[x]+y''''[x]==0)
ode[[1538]]=(-Cosh[a x]+a^4 y[x]+2 a^2 y''[x]+y''''[x]==0)
ode[[1539]]=(a^4 lambda y[x]+a^2 (1+lambda) y''[x]+y''''[x]==0)
ode[[1540]]=(lambda y[x]+a b y'[x]+a (-1+b x) y''[x]+y''''[x]==0)
ode[[1541]]=((EulerGamma+beta lambda+a x^2) y[x]+(c+b lambda+a x^2) y''[x]+y''''[x]==0)
ode[[1542]]=(b D[WeierstrassP[x,{g2,g3}],x] y'[x]+y[x] (d+c D[WeierstrassP[x,{g2,g3}],{x,2}])+a WeierstrassP[x,{g2,g3}] y''[x]+y''''[x]==0)
ode[[1543]]=((beta+alpha JacobiSN[z,x]^2) y[x]+b y'[x]-(a+12 k^2 JacobiSN[z,x]^2) y''[x]+y''''[x]==0)
ode[[1544]]=(10 f'[x] y'[x]+y[x] (3 f[x]^2+3 f''[x])+10 f[x] y''[x]+y''''[x]==0)
ode[[1545]]=(24 Cos[2 x]-32 Sin[2 x]+4 y[x]-4 y'[x]-3 y''[x]+2 y'''[x]+y''''[x]==0)
ode[[1546]]=(a^4 x^4 y[x]+4 a^3 x^3 y'[x]+6 a^2 x^2 y''[x]+4 a x y'''[x]+y''''[x]==0)
ode[[1547]]=(y'[x] (6 f[x]^3+30 f[x] g[x]+7 f[x] f'[x]+10 g'[x]+f''[x])+3 y[x] (6 f[x]^2 g[x]+3 g[x]^2+2 g[x] f'[x]+5 f[x] g'[x]+g''[x])+(11 f[x]^2+10 g[x]+4 f'[x]) y''[x]+6 f[x] y'''[x]+y''''[x]==0)
ode[[1548]]=(-4 Cos[x]-3 y'[x]+11 y''[x]-12 y'''[x]+4 y''''[x]==0)
ode[[1549]]=(-24+5 y'''[x]+x y''''[x]==0)
ode[[1550]]=(2 x^3 (-3+x^2) y[x]-x^2 (-7+9 x^2) y'[x]+12 x^3 y''[x]-(1+6 x^2) y'''[x]+x y''''[x]==0)
ode[[1551]]=(nu^2 (4+nu^2 x^2) y[x]-2 (6+nu^2 x^2) y''[x]+x^2 y''''[x]==0)
ode[[1552]]=(-b x^2+a y[x]+2 x y'''[x]+x^2 y''''[x]==0)
ode[[1553]]=(2 y''[x]+4 x y'''[x]+x^2 y''''[x]==0)
ode[[1554]]=(6 y''[x]+6 x y'''[x]+x^2 y''''[x]==0)
ode[[1555]]=(-lambda^2 y[x]+6 y''[x]+6 x y'''[x]+x^2 y''''[x]==0)
ode[[1556]]=(12 y''[x]+8 x y'''[x]+x^2 y''''[x]==0)
ode[[1557]]=(-lambda^2 y[x]+12 y''[x]+8 x y'''[x]+x^2 y''''[x]==0)
ode[[1558]]=(-(1/16) b^4 y[x]+(1+n-nu) (2+n-nu) y''[x]+(4+2 n-2 nu) x y'''[x]+x^2 y''''[x]==0)
ode[[1559]]=(-a^4 x^3 y[x]+y'[x]-x y''[x]+2 x^2 y'''[x]+x^3 y''''[x]==0)
ode[[1560]]=(6 x y''[x]+6 x^2 y'''[x]+x^3 y''''[x]==0)
ode[[1561]]=(((-2+n) n (1+n) (3+n)+a x^4) y[x]+4 n (1+n) x y'[x]-2 n (1+n) x^2 y''[x]+x^4 y''''[x]==0)
ode[[1562]]=(-4 x^4 y[x]+(-1+4 n^2) x y'[x]-(-1+4 n^2) x^2 y''[x]+4 x^3 y'''[x]+x^4 y''''[x]==0)
ode[[1563]]=((-1+4 n^2-4 x^4) y[x]-(-1+4 n^2) x y'[x]-(-1+4 n^2) x^2 y''[x]+4 x^3 y'''[x]+x^4 y''''[x]==0)
ode[[1564]]=(-(-3+12 n^2+4 x^4) y[x]+(-3+12 n^2) x y'[x]-(3+4 n^2) x^2 y''[x]+4 x^3 y'''[x]+x^4 y''''[x]==0)
ode[[1565]]=((rho^2 sigma^2+8 x^2) y[x]+((1-rho^2-sigma^2) x+16 x^3) y'[x]+((7-rho^2-sigma^2) x^2+4 x^4) y''[x]+6 x^3 y'''[x]+x^4 y''''[x]==0)
ode[[1566]]=(((mu^2-nu^2)^2+8 x^2) y[x]+((1-2 mu^2-2 nu^2) x+16 x^3) y'[x]+((7-2 mu^2-2 nu^2) x^2+4 x^4) y''[x]+6 x^3 y'''[x]+x^4 y''''[x]==0)
ode[[1567]]=(12 x^2 y''[x]+8 x^3 y'''[x]+x^4 y''''[x]==0)(*34*)
ode[[1568]]=(a y[x]+12 x^2 y''[x]+8 x^3 y'''[x]+x^4 y''''[x]==0)(*35*)
ode[[1569]]=(x^4*y''''[x]+(6-4*a)*x^3*y'''[x]+(4*b^2*c^2*x^(2*c)+6*(a-1)^2-2*c^2*(mu^2+nu^2)+1)*x^2*y''[x]+(4*(3*c-2*a+1)*b^2*c^2*x^(2*c)+(2*a-1)*(2*c^2*(mu^2+nu^2)-2*a*(a-1)-1))*x*y'[x]+(4*((a-c)*(a-2*c))*b^2*c^2*x^(2*c)+(mu*c+nu*c+a)*(mu*c+nu*c-a)*(mu*c-nu*c+a)*(mu*c-nu*c-a))*y[x]==0)
ode[[1570]]=(((-a+c mu-c nu) (a+c mu-c nu) (-a+c mu+c nu) (a+c mu+c nu)+4 b^2 (a-2 c) (a-c) c^2 x^(2 c)) y[x]+x ((-1+2 a) (-1-2 (-1+a) a+2 c^2 (mu^2+nu^2))+4 b^2 c^2 (1-2 a+3 c) x^(2 c)) y'[x]+x^2 (1+6 (-1+a)^2-2 c^2 (mu^2+nu^2)+4 b^2 c^2 x^(2 c)) y''[x]+(6-4 a) x^3 y'''[x]+x^4 y''''[x]==0)
ode[[1571]]=(((a^2-c^2 nu^2) (a^2+4 a c+4 c^2-c^2 nu^2)-b^4 c^4 x^(4 c)) y[x]+(-1+2 a+2 c) (-2 a^2-(-1+2 a) (-1+2 c)+2 c^2 nu^2) x y'[x]+(-1+2 a^2+4 (-1+a) (-1+c)+4 (-1+a+c)^2-2 c^2 nu^2) x^2 y''[x]+(6-4 a-4 c) x^3 y'''[x]+x^4 y''''[x]==0)
ode[[1572]]=(-(1/16) b^4 x^(2/v) y[x]+(-1+nu) nu^2 (-1+2 nu) x^2 y''[x]+nu^3 (-2+4 nu) x^3 y'''[x]+nu^4 x^4 y''''[x]==0)
ode[[1573]]=((-2 mu (1+mu)-2 nu (1+nu)+(mu (1+mu)-nu (1+nu))^2) y[x]-6 (-2+mu (1+mu)+nu (1+nu)) x y'[x]+(-8+24 x^3-2 (mu (1+mu)+nu (1+nu)) (-1+x^2)) y''[x]+10 x (-1+x^2) y'''[x]+(-1+x^2)^2 y''''[x]==0)
ode[[1574]]=(-(1/x^5)+E^x y[x]+4 E^x y'[x]+6 E^x y''[x]+4 (2+E^x) y'''[x]+(E^x+2 x) y''''[x]==0)
ode[[1575]]=(-f[x]+Sin[x]^6 y[x]-4 Cos[x] Sin[x]^5 y'[x]-6 Sin[x]^6 y''[x]+4 Cos[x] Sin[x]^5 y'''[x]+Sin[x]^6 y''''[x]==0)
ode[[1576]]=(2 f'[x] (-a^2 y'[x]+y'''[x])+f[x] (a^4 y[x]-2 a^2 y''[x]+y''''[x])==0)
ode[[1577]]=(f''[x] y''[x]+2 f'[x] y'''[x]+f[x] y''''[x]==0)
ode[[1578]]=(a^4 y[x]-2 a^2 y''[x]-lambda (-b+a x) (-a^2 y[x]+y''[x])+y''''[x]==0)
(*chapter 5*)
ode[[1579]]=(-a x-c Cos[x]-b Sin[x]+y'[x]+2 y'''[x]+D[y[x],{x,5}]==0)
ode[[1580]]=(-Sin[1/2 x] Sin[3/2 x]+y[x]+D[y[x],{x,5+1}]==0)
ode[[1581]]=(-b-a x y[x]+D[y[x],{x,5}]==0)
ode[[1582]]=(a nu x^(-1+nu) y[x]+a x^nu y'[x]+D[y[x],{x,5}]==0)
ode[[1583]]=(-f[x]+a D[y[x],{x,5-1}]+D[y[x],{x,5}]==0)
ode[[1584]]=(a x y[x]-n m D[y[x],{x,5-1}]+x D[y[x],{x,5}]==0)
ode[[1585]]=(x y[x] (a y'[x]+b y''[x]+c D[y[x],{x,5-2}]+e D[y[x],{x,5-1}])==0)
ode[[1586]]=(x*D[y[x],{x,5}]-(a*A[1]-A[0])*x-A[1]-((a*A[2]-A[1])*x+A[2])*y'[x]-((a*A[3]-A[2])*x+A[3])*y''[x]-((a*A[4]-A[3])*x+A[4])*y'''[x]-((a*A[5]-A[4])*x+A[5])*y''''[x]==0)
ode[[1587]]=(-a y[x]+x^5 D[y[x],{x,2 5}]==0)
ode[[1588]]=(-a y[x]+x^(2 5) D[y[x],{x,5}]==0)
ode[[1589]]=(-a y[x]+x^(5+1/2) D[y[x],{x,2 5 +1}]==0)
ode[[1590]]=(-c y[x]+(-a+x)^5 (-b+x)^5 D[y[x],{x,5}]==0)
(*chapter 6*)
ode[[1591]]=(-y[x]^2+y''[x]==0)
ode[[1592]]=(-6 y[x]^2+y''[x]==0)
ode[[1593]]=(-x-6 y[x]^2+y''[x]==0)
ode[[1594]]=(4 y[x]-6 y[x]^2+y''[x]==0)
ode[[1595]]=(c+b x+a y[x]^2+y''[x]==0)
ode[[1596]]=(a-x y[x]-2 y[x]^3+y''[x]==0)
ode[[1597]]=(-a y[x]^3+y''[x]==0)
ode[[1598]]=(-b+2 a b x y[x]-2 a^2 y[x]^3+y''[x]==0)
ode[[1599]]=(d+c y[x]+b x y[x]+a y[x]^3+y''[x]==0)
ode[[1600]]=(d+c y[x]+b y[x]^2+a y[x]^3+y''[x]==0)
ode[[1601]]=(a x^r y[x]^n+y''[x]==0)
ode[[1602]]=(-y[x]+a^(2 n) (1+n) y[x]^(1+2 n)+y''[x]==0)
ode[[1603]]=(-(1/(k+e x+c x^2+d y[x]+b x y[x]+a y[x]^2)^(3/2))+y''[x]==0)
ode[[1604]]=(-E^y[x]+y''[x]==0)
ode[[1605]]=(a E^x Sqrt[y[x]]+y''[x]==0)
ode[[1606]]=(E^x Sin[y[x]]+y''[x]==0)
ode[[1607]]=(a Sin[y[x]]+y''[x]==0)
ode[[1608]]=(-b Sin[x]+a^2 Sin[y[x]]+y''[x]==0)
ode[[1609]]=(-b f[x]+a^2 Sin[y[x]]+y''[x]==0)
ode[[1610]]=(-(h[y[x]/Sqrt[x]]/x^(3/2))+y''[x]==0)
ode[[1611]]=(-2 y[x]-y[x]^2-3 y'[x]+y''[x]==0)
ode[[1612]]=(12 y[x]-y[x]^(3/2)-7 y'[x]+y''[x]==0)
ode[[1613]]=(6 a^2 y[x]-6 y[x]^2+n a y'[x]+y''[x]==0)
ode[[1614]]=(2 a^2 y[x]-2 y[x]^3+3 a y'[x]+y''[x]==0)
ode[[1615]]=(-((2 (1+n) (2+n) y[x] (-1+y[x]^(n/(1+n))))/n^2)-((4+3 n) y'[x])/n+y''[x]==0)
ode[[1616]]=(1/4 (-1+a^2) y[x]+b y[x]^n+a y'[x]+y''[x]==0)
ode[[1617]]=(b x^r y[x]^n+a y'[x]+y''[x]==0)
ode[[1618]]=(-2 a+b E^y[x]+a y'[x]+y''[x]==0)
ode[[1619]]=(f[x] Sin[y[x]]+a y'[x]+y''[x]==0)
ode[[1620]]=(-y[x]^3+y[x] y'[x]+y''[x]==0)
ode[[1621]]=(a y[x]-y[x]^3+y[x] y'[x]+y''[x]==0)
ode[[1622]]=(2 a^2 y[x]+a y[x]^2-y[x]^3+(3 a+y[x]) y'[x]+y''[x]==0)
ode[[1623]]=(f[x] y[x]^2-y[x]^3+y[x] (2 f[x]^2+f'[x])+(3 f[x]+y[x]) y'[x]+y''[x]==0)
ode[[1624]]=(b f[x]^3-y[x]^3+y[x] y'[x]-(f[x]+f'[x]/f[x]) (y[x]^2+3 y'[x])+y[x] (a f[x]^2+3 f'[x]+(3 f'[x]^2)/f[x]^2-f''[x]/f[x])+y''[x]==0)
ode[[1625]]=(-y[x]^3-(y[x]^2 f'[x])/(2 f[x])+(y[x]-(3 f'[x])/(2 f[x])) y'[x]+y[x] (f[x]+f'[x]^2/f[x]^2-f''[x]/(2 f[x]))+y''[x]==0)
ode[[1626]]=(y[x] f'[x]+f[x] y'[x]+2 y[x] y'[x]+y''[x]==0)
ode[[1627]]=(-g[x]+2 y[x] y'[x]+f[x] (y[x]^2+y'[x])+y''[x]==0)
ode[[1628]]=(-g[x]+f[x] y[x]+y[x]^3+3 y[x] y'[x]+y''[x]==0)
ode[[1629]]=(f[x] y[x]^2+y[x]^3+(f[x]+3 y[x]) y'[x]+y''[x]==0)
ode[[1630]]=(-b-4 a^2 y[x]-3 a y[x]^2-3 y[x] y'[x]+y''[x]==0)
ode[[1631]]=(f[x] y[x]^2+y[x]^3-(f[x]+3 y[x]) y'[x]+y''[x]==0)
ode[[1632]]=(-2 a y[x] y'[x]+y''[x]==0)
ode[[1633]]=(b y[x]^3+a y[x] y'[x]+y''[x]==0)
ode[[1634]]=(j[x,y[x]]+h[x,y[x]] y'[x]+y''[x]==0)
ode[[1635]]=(b y[x]+a y'[x]^2+y''[x]==0)
ode[[1636]]=(c y[x]+b y'[x]+a Abs[y'[x]] y'[x]+y''[x]==0)
ode[[1637]]=(c y[x]+b y'[x]+a y'[x]^2+y''[x]==0)
ode[[1638]]=(b Sin[y[x]]+a y'[x]^2+y''[x]==0)
ode[[1639]]=(b Sin[y[x]]+a Abs[y'[x]] y'[x]+y''[x]==0)
ode[[1640]]=(b y[x]+a y[x] y'[x]^2+y''[x]==0)
ode[[1641]]=(g[x] y'[x]+h[y[x]] y'[x]^2+y''[x]==0)
ode[[1642]]=(f[x] h[y[x]]+g[x] y'[x]-(j[y[x]] y'[x]^2)/h[y[x]]+y''[x]==0)
ode[[1643]]=(g[x] j[y[x]]+f[x] y'[x]+h[y[x]] y'[x]^2+y''[x]==0)
ode[[1644]]=(k[y[x]]+j[y[x]] y'[x]+h[y[x]] y'[x]^2+y''[x]==0)
ode[[1645]]=((j[x,y[x]]+h[x,y[x]] y'[x]) (1+y'[x]^2)+y''[x]==0)
ode[[1646]]=(a y[x] (1+y'[x]^2)^2+y''[x]==0)
ode[[1647]]=(-a (-y[x]+x y'[x])^r+y''[x]==0)
ode[[1648]]=(-k x^a y[x]^b y'[x]^c+y''[x]==0)
ode[[1649]]=(h[x,y[x]] (-(y[x]/x)+y'[x])^a+y''[x]==0)
ode[[1650]]=(-a Sqrt[1+y'[x]^2]+y''[x]==0)
ode[[1651]]=(-b-a Sqrt[1+y'[x]^2]+y''[x]==0)
ode[[1652]]=(-a Sqrt[b y[x]^2+y'[x]^2]+y''[x]==0)
ode[[1653]]=(-a (1+y'[x]^2)^(3/2)+y''[x]==0)
ode[[1654]]=(-2 a x (1+y'[x]^2)^(3/2)+y''[x]==0)
ode[[1655]]=(-a y[x] (1+y'[x]^2)^(3/2)+y''[x]==0)
ode[[1656]]=(-a (c+b x+y[x]) (1+y'[x]^2)^(3/2)+y''[x]==0)
ode[[1657]]=(y[x]^3 y'[x]-y[x] y'[x] Sqrt[y[x]^4+4 y'[x]]+y''[x]==0)
ode[[1658]]=(-h[y'[x],a x+b y[x]]+y''[x]==0)
ode[[1659]]=(-h[x,y'[x]/y[x]] y[x]+y''[x]==0)
ode[[1660]]=(-x^(-2+n) h[x^-n y[x],x^(1-n) y'[x]]+y''[x]==0)
ode[[1661]]=(9 y'[x]^4+8 y''[x]==0)
ode[[1662]]=(h[y'[x]]+c y[x]+a y''[x]==0)
ode[[1663]]=(-x y[x]^n+2 y'[x]+x y''[x]==0)
ode[[1664]]=(a x^m y[x]^n+2 y'[x]+x y''[x]==0)
ode[[1665]]=(E^y[x] x+2 y'[x]+x y''[x]==0)
ode[[1666]]=(b E^y[x] x+a y'[x]+x y''[x]==0)
ode[[1667]]=(b E^y[x] x^(n-2 a)+a y'[x]+x y''[x]==0)
ode[[1668]]=(-(1-y[x]) y'[x]+x y''[x]==0)
ode[[1669]]=(y[x]^2+2 y'[x]-x^2 y'[x]^2+x y''[x]==0)
ode[[1670]]=(-b+a (-y[x]+x y'[x])^2+x y''[x]==0)
ode[[1671]]=(y'[x]+y'[x]^3+2 x y''[x]==0)
ode[[1672]]=(-a (-y[x]+y[x]^n)+x^2 y''[x]==0)
ode[[1673]]=(a (-1+E^y[x])+x^2 y''[x]==0)
ode[[1674]]=((a (a+b)+b^2 c^2 x^(2 b)) y[x]-(-1+2 a+b) x y'[x]+x^2 y''[x]==0)
ode[[1675]]=(-x^k h[x^k y[x],k y[x]+x y'[x]]+(1+a) x y'[x]+x^2 y''[x]==0)
ode[[1676]]=(-b x^2+a (-y[x]+x y'[x])^2+x^2 y''[x]==0)
ode[[1677]]=(b x+a y[x] y'[x]^2+x^2 y''[x]==0)
ode[[1678]]=(-Sqrt[b y[x]^2+a x^2 y'[x]^2]+x^2 y''[x]==0)
ode[[1679]]=(1+y'[x]^2+(1+x^2) y''[x]==0)
ode[[1680]]=(4 y[x]-x^4 y'[x]^2+4 x^2 y''[x]==0)
ode[[1681]]=(2 y[x]+a y[x]^3+9 x^2 y''[x]==0)
ode[[1682]]=(24+12 x y[x]+x^3 (-y[x]^3+y[x] y'[x]+y''[x])==0)
ode[[1683]]=(-a (-y[x]+x y'[x])^2+x^3 y''[x]==0)
ode[[1684]]=(b+x y[x] (a+3 x y[x]-2 x^2 y[x]^2)+x^2 (9+2 x y[x]) y'[x]+2 x^3 y''[x]==0)
ode[[1685]]=(b+a x y[x]-(-12 x^2+k x^(-1+k)) (y[x]^2+3 y'[x])+2 (4 x^3-x^k) (-y[x]^3+y[x] y'[x]+y''[x])==0)
ode[[1686]]=(a^2 y[x]^n+x^4 y''[x]==0)
ode[[1687]]=(4 y[x]^2-x (x^2+2 y[x]) y'[x]+x^4 y''[x]==0)
ode[[1688]]=(4 y[x]^2-x^2 y'[x] (x+y'[x])+x^4 y''[x]==0)
ode[[1689]]=((-y[x]+x y'[x])^3+x^4 y''[x]==0)
ode[[1690]]=(-y[x]^(3/2)+Sqrt[x] y''[x]==0)
ode[[1691]]=(-f[y[x]/Sqrt[c+b x+a x^2]]+(c+b x+a x^2)^(3/2) y''[x]==0)
ode[[1692]]=(-y[x]^(((1+2 n)/(1+n)))+x^(n/(1+n)) y''[x]==0)
ode[[1693]]=(-h[y[x],f[x] y'[x]]+f[x] f'[x] y'[x]+f[x]^2 y''[x]==0)
ode[[1694]]=(-a+y[x] y''[x]==0)
ode[[1695]]=(-a x+y[x] y''[x]==0)
ode[[1696]]=(-a x^2+y[x] y''[x]==0)
ode[[1697]]=(-a+y'[x]^2+y[x] y''[x]==0)
ode[[1698]]=(-b-a x+y'[x]^2+y[x] y''[x]==0)
ode[[1699]]=(-y'[x]+y'[x]^2+y[x] y''[x]==0)
ode[[1600]]=(1-y'[x]^2+y[x] y''[x]==0)
ode[[1701]]=(-1-y'[x]^2+y[x] y''[x]==0)
ode[[1702]]=(E^x y[x] (d+c y[x]^2)+E^(2 x) (b+a y[x]^4)-y'[x]^2+y[x] y''[x]==0)
ode[[1703]]=(-Log[y[x]] y[x]^2-y'[x]^2+y[x] y''[x]==0)
ode[[1704]]=(y[x] y''[x]-y'[x]^2 -y'[x] + f[x] y[x]^3 + y[x]^2 D[ f'[x]/f[x],x]==0)
ode[[1705]]=(-y[x]^3-y[x] f'[x]+f[x] y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1706]]=(f[x] y[x]^3-y[x]^4+f'[x] y'[x]-y'[x]^2-y[x] f''[x]+y[x] y''[x]==0)
ode[[1707]]=(b y[x]^2+a y[x] y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1708]]=(-2 a y[x]^2+b y[x]^3+a y[x] y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1709]]=(a y[x]+2 a^2 y[x]^2-2 b^2 y[x]^3-(-1+a y[x]) y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1710]]=(-y[x] (1+y[x]) (-a^2+b^2 y[x]^2)+(-1+a y[x]) y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1711]]=((Cos[x]^2-n^2 Cot[x]^2) Log[y[x]] y[x]^2+(Cot[x]+Tan[x]) y[x] y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1712]]=(-g[x] y[x]^2-f[x] y[x] y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1713]]=(-y[x] (-y[x]^2 f'[x]+g'[x])+(g[x]+f[x] y[x]^2) y'[x]-y'[x]^2+y[x] y''[x]==0)
ode[[1714]]=(-y[x]^2+3 y[x] y'[x]-3 y'[x]^2+y[x] y''[x]==0)
ode[[1715]]=(-a y'[x]^2+y[x] y''[x]==0)
ode[[1716]]=(a (1+y'[x]^2)+y[x] y''[x]==0)
ode[[1717]]=(b y[x]^3+a y'[x]^2+y[x] y''[x]==0)
ode[[1718]]=(c y[x]^2+d y[x]^(1-a)+b y[x] y'[x]+a y'[x]^2+y[x] y''[x]==0)
ode[[1719]]=(g[x] y[x]^2+f[x] y[x] y'[x]+a y'[x]^2+y[x] y''[x]==0)
ode[[1720]]=(c y[x]^4+b y[x]^2 y'[x]+a y'[x]^2+y[x] y''[x]==0)
ode[[1721]]=((a f[x]^2 y[x]^4)/(2+a)^2-(a y[x]^3 f'[x])/(2+a)-f[x] y[x]^2 y'[x]-((-1+a) y'[x]^2)/a+y[x] y''[x]==0)
ode[[1722]]=(-1-y'[x]^2-2 a y[x] (1+y'[x]^2)^(3/2)+y[x] y''[x]==0)
ode[[1723]]=(-y'[x]+y'[x]^2+(x+y[x]) y''[x]==0)
ode[[1724]]=(2 y'[x] (1+y'[x])+(x-y[x]) y''[x]==0)
ode[[1725]]=(-(1+y'[x]) (1+y'[x]^2)+(x-y[x]) y''[x]==0)
ode[[1726]]=(-h[y'[x]]+(x-y[x]) y''[x]==0)
ode[[1727]]=(1+y'[x]^2+2 y[x] y''[x]==0)
ode[[1728]]=(a-y'[x]^2+2 y[x] y''[x]==0)
ode[[1729]]=(a+f[x] y[x]^2-y'[x]^2+2 y[x] y''[x]==0)
ode[[1730]]=(-8 y[x]^3-y'[x]^2+2 y[x] y''[x]==0)
ode[[1731]]=(-4 y[x]^2-8 y[x]^3-y'[x]^2+2 y[x] y''[x]==0)
ode[[1732]]=(-4 y[x]^2 (x+2 y[x])-y'[x]^2+2 y[x] y''[x]==0)
ode[[1733]]=(y[x]^2 (b+a y[x])-y'[x]^2+2 y[x] y''[x]==0)
ode[[1734]]=(1+2 x y[x]^2+a y[x]^3-y'[x]^2+2 y[x] y''[x]==0)
ode[[1735]]=(y[x]^2 (b x+a y[x])-y'[x]^2+2 y[x] y''[x]==0)
ode[[1736]]=(-3 y[x]^4-y'[x]^2+2 y[x] y''[x]==0)
ode[[1737]]=(b-4 (a+x^2) y[x]^2-8 x y[x]^3-3 y[x]^4-y'[x]^2+2 y[x] y''[x]==0)
ode[[1738]]=(-8 y[x]^3+2 y[x]^2 (f[x]^2+f'[x])+3 f[x] y[x] y'[x]-y'[x]^2+2 y[x] y''[x]==0)
ode[[1739]]=(1+f[x] y[x]^2+y[x]^4+4 y[x]^2 y'[x]-y'[x]^2+2 y[x] y''[x]==0)
ode[[1740]]=(-3 y'[x]^2+2 y[x] y''[x]==0)
ode[[1741]]=(-4 y[x]^2-3 y'[x]^2+2 y[x] y''[x]==0)
ode[[1742]]=(f[x] y[x]^2-3 y'[x]^2+2 y[x] y''[x]==0)
ode[[1743]]=(y[x]^2 (1+a y[x]^3)-6 y'[x]^2+2 y[x] y''[x]==0)
ode[[1744]]=(-y'[x]^2 (1+y'[x]^2)+2 y[x] y''[x]==0)
ode[[1745]]=(1+y'[x]^2+2 (-a+y[x]) y''[x]==0)
ode[[1746]]=(-c-b x-a x^2-2 y'[x]^2+3 y[x] y''[x]==0)
ode[[1747]]=(-n y'[x]^2+3 y[x] y''[x]==0)
ode[[1748]]=(4 y[x]-3 y'[x]^2+4 y[x] y''[x]==0)
ode[[1749]]=(-12 y[x]^3-3 y'[x]^2+4 y[x] y''[x]==0)
ode[[1750]]=(c y[x]+b y[x]^2+a y[x]^3-3 y'[x]^2+4 y[x] y''[x]==0)
ode[[1751]]=(f[x] y[x]+g[x] y[x]^2+y[x]^4-2 y[x]^2 y'[x]+(6 y[x]^2-(2 y[x] f'[x])/f[x]) y'[x]-3 y'[x]^2+4 y[x] y''[x]==0)
ode[[1752]]=(a y[x]^2-n y'[x]^2+4 y[x] y''[x]==0)
ode[[1753]]=(8 y[x]^3-15 y'[x]^2+12 y[x] y''[x]==0)
ode[[1754]]=(-(-1+n) y'[x]^2+n y[x] y''[x]==0)
ode[[1755]]=(c0+c1 y[x]+c2 y[x]^2+c3 y[x]^3+c4 y[x]^4+b y'[x]^2+a y[x] y''[x]==0)
ode[[1756]]=(-((y[x] y'[x])/Sqrt[c^2+x^2])+b y'[x]^2+a y[x] y''[x]==0)
ode[[1757]]=(f[x]^2 y[x]^4+(2+a) f[x] y[x]^2 y'[x]+a y[x]^3 y'[x]-(-1+a) y'[x]^2+a y[x] y''[x]==0)
ode[[1758]]=(c y'[x]^2+(b+a y[x]) y''[x]==0)
ode[[1759]]=(-y[x] y'[x]+x y'[x]^2+x y[x] y''[x]==0)
ode[[1760]]=(f[x]+a y[x] y'[x]+x y'[x]^2+x y[x] y''[x]==0)
ode[[1761]]=(y[x] (c+b y[x]^2)+x (d+a y[x]^4)+y[x] y'[x]-x y'[x]^2+x y[x] y''[x]==0)
ode[[1762]]=(b x y[x]^3+a y[x] y'[x]-x y'[x]^2+x y[x] y''[x]==0)
ode[[1763]]=(a y[x] y'[x]+2 x y'[x]^2+x y[x] y''[x]==0)
ode[[1764]]=((1+y[x]) y'[x]-2 x y'[x]^2+x y[x] y''[x]==0)
ode[[1765]]=(a y[x] y'[x]-2 x y'[x]^2+x y[x] y''[x]==0)
ode[[1766]]=(4 y[x] y'[x]-4 x y'[x]^2+x y[x] y''[x]==0)
ode[[1767]]=(-y[x] y'[x]+(-x+(a x)/Sqrt[b^2-x^2]) y'[x]^2+x y[x] y''[x]==0)
ode[[1768]]=(-y[x]+(x-y[x]) y'[x]+x y'[x]^2+x (x+y[x]) y''[x]==0)
ode[[1769]]=(y[x] y'[x]-x y'[x]^2+2 x y[x] y''[x]==0)
ode[[1770]]=(-2 (-1+y[x])^2 y[x]-2 x (-1+y[x]) y'[x]-2 x^2 y'[x]^2+x^2 (-1+y[x]) y''[x]==0)
ode[[1771]]=(-(-y[x]+x y'[x])^2+x^2 (x+y[x]) y''[x]==0)
ode[[1772]]=(a (-y[x]+x y'[x])^2+x^2 (x-y[x]) y''[x]==0)
ode[[1773]]=(y[x]^2-x^2 (1+y'[x]^2)+2 x^2 y[x] y''[x]==0)
ode[[1774]]=(d y[x]^2+c x y[x] y'[x]+b x^2 y'[x]^2+a x^2 y[x] y''[x]==0)
ode[[1775]]=(-a (2+x) y[x]^2+2 (1+x)^2 y[x] y'[x]-x (1+x)^2 y'[x]^2+x (1+x)^2 y[x] y''[x]==0)
ode[[1776]]=(3 x y[x]^2-12 x^2 y[x] y'[x]-4 (1-x^3) y'[x]^2+8 (1-x^3) y[x] y''[x]==0)
ode[[1777]]=(f3[x] y[x]^2+f2[x] y[x] y'[x]+f1[x] y'[x]^2+f0[x] y[x] y''[x]==0)
ode[[1778]]=(-a+y[x]^2 y''[x]==0)
ode[[1779]]=(a x+y[x] y'[x]^2+y[x]^2 y''[x]==0)
ode[[1780]]=(-b-a x+y[x] y'[x]^2+y[x]^2 y''[x]==0)
ode[[1781]]=((1-2 y[x]) y'[x]^2+(1+y[x]^2) y''[x]==0)
ode[[1782]]=(-3 y[x] y'[x]^2+(1+y[x]^2) y''[x]==0)
ode[[1783]]=(-2 (x-y[x]^2) y'[x]^3+y'[x] (1+4 y[x] y'[x])+(x+y[x]^2) y''[x]==0)
ode[[1784]]=(-(-y[x]+x y'[x]) (1+y'[x]^2)+(x^2+y[x]^2) y''[x]==0)
ode[[1785]]=(-2 (-y[x]+x y'[x]) (1+y'[x]^2)+(x^2+y[x]^2) y''[x]==0)
ode[[1786]]=(f[x] (1-y[x]) y[x] y'[x]-(1-2 y[x]) y'[x]^2+2 (1-y[x]) y[x] y''[x]==0)
ode[[1787]]=(h[y[x]]-(1-3 y[x]) y'[x]^2+2 (1-y[x]) y[x] y''[x]==0)
ode[[1788]]=(-4 (1-y[x]) y[x]^2 (-f[x]^2+g[x]^2-f'[x]-g'[x])+4 y[x] (g[x]+f[x] y[x]) y'[x]+(1-3 y[x]) y'[x]^2-2 (1-y[x]) y[x] y''[x]==0)
ode[[1789]]=((1-y[x])^3 (-f1[x]^2+f0[x]^2 y[x]^2)+4 (1-y[x]) y[x]^2 (f[x]^2-g[x]^2-f'[x]-g'[x])-4 y[x] (g[x]+f[x] y[x]) y'[x]+(1-3 y[x]) y'[x]^2-2 (1-y[x]) y[x] y''[x]==0)
ode[[1790]]=(-h[y[x]]-2 (1-2 y[x]) y'[x]^2+3 (1-y[x]) y[x] y''[x]==0)
ode[[1791]]=(-h[y[x]]-3 (1-2 y[x]) y'[x]^2+(1-y[x]) y''[x]==0)
ode[[1792]]=(h[y[x]]+(c+b y[x]) y'[x]^2+a (-1+y[x]) y[x] y''[x]==0)
ode[[1793]]=(f[x] (-1+y[x]) y[x] y'[x]-(-1+a) (-1+2 y[x]) y'[x]^2+a (-1+y[x]) y[x] y''[x]==0)
ode[[1794]]=(f[x] (-1+y[x]) y[x] y'[x]-((1-a) b+(-a-b+2 a b) y[x]) y'[x]^2+a b (-1+y[x]) y[x] y''[x]==0)
ode[[1795]]=(-a+x y[x]^2 y''[x]==0)
ode[[1796]]=(-x (a^2-y[x]^2) y'[x]+(a^2-x^2) y[x] y'[x]^2+(a^2-x^2) (a^2-y[x]^2) y''[x]==0)
ode[[1797]]=(c x (-1+y[x]) y[x]^2+d x^2 y[x]^2 (1+y[x])+(-1+y[x])^3 (b+a y[x]^2)+2 x (-1+y[x]) y[x] y'[x]-x^2 (-1+3 y[x]) y'[x]^2+2 x^2 (-1+y[x]) y[x] y''[x]==0)
ode[[1798]]=((x+y[x]) (-y[x]+x y'[x])^3+x^3 y[x]^2 y''[x]==0)
ode[[1799]]=(-a+y[x]^3 y''[x]==0)
ode[[1800]]=((1-3 y[x]^2) y'[x]^2+y[x] (1+y[x]^2) y''[x]==0)
ode[[1801]]=(-1-a^2 x y[x]^2+y[x]^4+2 y[x]^3 y''[x]==0)
ode[[1802]]=(-c-b x-a x^2+y[x]^2 y'[x]^2+2 y[x]^3 y''[x]==0)
ode[[1803]]=(-a3 (a-y[x])^2 (b-y[x])^2-a2 (a-y[x])^2 (c-y[x])^2-a1 (b-y[x])^2 (c-y[x])^2-a0 (a-y[x])^2 (b-y[x])^2 (c-y[x])^2+((a-y[x]) (b-y[x])+(a-y[x]) (c-y[x])+(b-y[x]) (c-y[x])) y'[x]^2+2 (a-y[x]) (b-y[x]) (c-y[x]) y''[x]==0)
ode[[1804]]=(-(-(a/2)+6 y[x]^2) y'[x]^2+(-b-a y[x]+4 y[x]^3) y''[x]==0)
ode[[1805]]=(-(-(a/2)+6 y[x]^2) y'[x]^2+(-b-a y[x]+4 y[x]^3) (f[x] y'[x]+y''[x])==0)
ode[[1806]]=(-(1-y[x])^2 y[x]^2-f[x] ((-1+y[x]) y[x] (-x+y[x]))^(3/2)+2 (1-y[x]) y[x] (x^2+y[x]-2 x y[x]) y'[x]+(1-x) x (x-2 y[x]-2 x y[x]+3 y[x]^2) y'[x]^2-2 (1-x) x (1-y[x]) (x-y[x]) y[x] y''[x]==0)
ode[[1807]]=(b x (1-y[x])^2 (x-y[x])^2-d (1-x) x (1-y[x])^2 y[x]^2-c (1-x) (x-y[x])^2 y[x]^2+a (1-y[x])^2 (x-y[x])^2 y[x]^2-2 (1-x) x (1-y[x]) y[x] (x^2+y[x]-2 x y[x]) y'[x]-(1-x)^2 x^2 (x-2 y[x]-2 x y[x]+3 y[x]^2) y'[x]^2+2 (1-x)^2 x^2 (1-y[x]) (x-y[x]) y[x] y''[x]==0)
ode[[1808]]=(y[x] (1+a^2-2 a^2 y[x]^2) y'[x]^2+b Sqrt[(1-y[x]^2) (1-a^2 y[x]^2)] y'[x]^2+(-1+y[x]^2) (-1+a^2 y[x]^2) y''[x]==0)
ode[[1809]]=(d y[x]+(c+2 b x+a x^2+y[x]^2)^2 y''[x]==0)
ode[[1810]]=(-a+Sqrt[y[x]] y''[x]==0)
ode[[1811]]=(-a (1+y'[x]^2)^(3/2)+Sqrt[x^2+y[x]^2] y''[x]==0)
ode[[1812]]=((1+Log[y[x]]) y'[x]^2+(1-Log[y[x]]) y[x] y''[x]==0)
ode[[1813]]=(A (c+a Sin[y[x]]^2) y[x]+a Cos[y[x]] Sin[y[x]] y'[x]^2+(b+a Sin[y[x]]^2) y''[x]==0)
ode[[1814]]=(j[y[x]]+a h[y[x]] y'[x]^2+h[y[x]] y''[x]==0)
ode[[1815]]=(-h[y[x]]^2 j[x,y'[x]/h[y[x]]]-h[y[x]] y'[x]^2+h[y[x]] y''[x]==0)
ode[[1816]]=(-x y[x]^2-x^2 y[x] y'[x]+y'[x] y''[x]==0)
ode[[1817]]=(4 y'[x]^2+(-y[x]+x y'[x]) y''[x]==0)
ode[[1818]]=(-(1+y'[x]^2)^2+(-y[x]+x y'[x]) y''[x]==0)
ode[[1819]]=(b y[x]^2+a x^3 y'[x] y''[x]==0)
ode[[1820]]=(f5[x] y[x]^2+f4[x] y[x] y'[x]+f3[x] y'[x]^2+(f2[x] y[x]+f1[x] y'[x]) y''[x]==0)
ode[[1821]]=(y[x]+3 x y'[x]+2 y[x] y'[x]^3+(x^2+2 y[x]^2 y'[x]) y''[x]==0)
ode[[1822]]=(y[x]^3+(y[x]^2+y'[x]^2) y''[x]==0)
ode[[1823]]=(-b+(y'[x]^2+a (-y[x]+x y'[x])) y''[x]==0)
ode[[1824]]=(-1-y'[x]^2+(-x y'[x]+a Sqrt[1+y'[x]^2]) y''[x]==0)
ode[[1825]]=(f[x]+j[y[x]] y'[x]+h[y'[x]] y''[x]==0)
ode[[1826]]=(-b-a y[x]+y''[x]^2==0)
ode[[1827]]=(y'[x]-2 a x y''[x]+a^2 y''[x]^2==0)
ode[[1828]]=(-2 y[x]+2 y'[x] (x+y'[x])-x (x+4 y'[x]) y''[x]+2 (1+x^2) y''[x]^2==0)
ode[[1829]]=(4 y'[x]^2-2 (y[x]+3 x y'[x]) y''[x]+3 x^2 y''[x]^2==0)
ode[[1830]]=(-36 x y'[x]^2+6 y[x] y''[x]-6 (1-6 x) x y'[x] y''[x]+(2-9 x) x^2 y''[x]^2==0)
ode[[1831]]=(F[1,1](x)*y''[x]+((F[2,1](x)+F[1,2](x))*y''[x] +y[x]*(F[1,0](x)+F[0,1](x)))*y'[x]+F[2,2](x)*y''[x]^2+y[x]*(F[2,0](x)+F[0,2](x))*y''[x]+F[0,0](x)*y[x]^2==0)
(*(F[0,0][x] y[x]^2+F[1,1][x] y'[x]^2+y[x] (F[0,2][x]+F[2,0][x]) y''[x]+F[2,2][x] y''[x]^2+y'[x] (y[x] (F[0,1][x]+F[1,0][x])+(F[1,2][x]+F[2,1][x]) y''[x])==0)*)
ode[[1832]]=(-a E^(2 x)+y[x] y''[x]^2==0)
ode[[1833]]=(y'[x]^2 (-1+a^2 y'[x]^2)-2 a^2 y[x] y'[x]^2 y''[x]+(-b^2+a^2 y[x]^2) y''[x]^2==0)
ode[[1834]]=(-4 x y[x] (-y[x]+x y'[x])^3+(y[x]^2-x^2 y'[x]^2+x^2 y[x] y''[x])^2==0)
ode[[1835]]=(32 y''[x] (-y'[x]+x y''[x])^3+(-y'[x]^2+2 y[x] y''[x])^3==0)
ode[[1836]]=(d y'[x]^2+c y[x] y''[x]+Sqrt[b y'[x]^2+a y''[x]^2]==0)
(*chapter 7*)
ode[[1837]]=(-a^2 (y'[x]+2 y'[x]^3+y'[x]^5)+y'''[x]==0)
ode[[1838]]=(1-y'[x]^2+y[x] y''[x]+y'''[x]==0)
ode[[1839]]=(y'[x]^2-y[x] y''[x]+y'''[x]==0)
ode[[1840]]=(a y[x] y''[x]+y'''[x]==0)
ode[[1841]]=(-f[x]+y[x]^2+(-1+2 x y[x]) y'[x]+x y''[x]+x^2 y'''[x]==0)
ode[[1842]]=((1-y[x]) y'[x]+x y'[x]^2+x (-1+y[x]) y''[x]+x^2 y'''[x]==0)
ode[[1843]]=(y[x]^3 y'[x]-y'[x] y''[x]+y[x] y'''[x]==0)
ode[[1844]]=(15 y'[x]^3-18 y[x] y'[x] y''[x]+4 y[x]^2 y'''[x]==0)
ode[[1845]]=(40 y'[x]^3-45 y[x] y'[x] y''[x]+9 y[x]^2 y'''[x]==0)
ode[[1846]]=(-3 y'[x]^2+2 y'[x] y'''[x]==0)
ode[[1847]]=(-3 y'[x] y''[x]^2+(1+y'[x]^2) y'''[x]==0)
ode[[1848]]=(-(a+3 y'[x]) y''[x]^2+(1+y'[x]^2) y'''[x]==0)
ode[[1849]]=(-a Sqrt[1+b^2 y''[x]^2]+y''[x] y'''[x]==0)
ode[[1850]]=(y'[x]^3 y'''[x]-y''[x] y'''[x]+y'[x] y''''[x]==0)
ode[[1851]]=(2 q[x] Sin[y[x]] y'[x]^2+y'[x]^3 (f'[x] y'[x]+f[x] y''[x])+Cos[y[x]] (-q'[x] y'[x]+q[x] y''[x])-y''[x] (y'[x] f''[x]+2 f'[x] y''[x]+f[x] y'''[x])+y'[x] (3 f''[x] y''[x]+y'[x] f'''[x]+3 f'[x] y'''[x]+f[x] y''''[x])==0)
ode[[1852]]=(-5 y'''[x]^2+3 y''[x] y''''[x]==0)
ode[[1853]]=(40 y'''[x]^3-45 y''[x] y'''[x] y''''[x]+9 y''[x]^2 y'''''[x]==0)