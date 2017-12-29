%
%  eats(Predator, Prey).
%
eats(bird,prawn).
eats(bird,mussels).
eats(bird,crab).
eats(bird,limpets).
eats(bird,whelk).
eats(crab,mussels).
eats(crab,limpets).
eats(fish,prawn).
eats(limpets,seaweed).
eats(lobster,crab).
eats(lobster,mussels).
eats(lobster,limpets).
eats(lobster,whelk).
eats(mussels,phytoplankton).
eats(mussels,zooplankton).
eats(prawn,zooplankton).
eats(whelk,limpets).
eats(whelk,mussels).
eats(zooplankton,phytoplankton).

%
%  eats(Predator, Prey).
%
eats(grasshopper,terrestrial_plants).
eats(harvest_mouse,grasshopper).
eats(harvest_mouse,terrestrial_plants).
eats(hawk,harvest_mouse).
eats(hawk,rat).
eats(hawk,shrew).
eats(hawk,vole).
eats(heron,shrimp).
eats(heron,smelt).
eats(mallard,grasshopper).
eats(mallard,terrestrial_plants).
eats(mallard,shrimp).
eats(owl,rat).
eats(owl,sparrow).
eats(owl,mallard).
eats(owl,sandpiper).
eats(rat,grasshopper).
eats(rat,terrestrial_plants).
eats(rat,sparrow).
eats(sandpiper,shrimp).
eats(shrew,grasshopper).
eats(shrimp,aquatic_plants).
eats(smelt,aquatic_plants).
eats(smelt,shrimp).
eats(sparrow,grasshopper).
eats(sparrow,terrestrial_plants).
eats(vole,terrestrial_plants).
eats(vole,grasshopper).



%
%  eats(Predator, Prey).
%
eats(lion,zebra).
eats(zebra,grass).




isProducer(X) :-
 \+ eats(X, _), !.


height(Organism, X) :-
 isProducer(Organism),
 X is 0.

height(Organism, X) :-
 eats(Organism, Y),
 height(Y, Yheight),
 X is Yheight + 1.


%% Bug here come and fix it later




















