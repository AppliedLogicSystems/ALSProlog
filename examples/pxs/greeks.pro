/*---------------------------------------------------------------*
 *	greeks.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	The family tree of the Greek gods
 *---------------------------------------------------------------*/


parent(uranus, cronus).
parent(gaea, cronus).
parent(uranus, rhea).
parent(gaea, rhea).
parent(rhea, zeus).
parent(cronus, hera).
parent(rhea, hera).
parent(cronus, hera).
parent(cronus, hades).
parent(rhea, hades).
parent(cronus, hestia).
parent(rhea, hestia).
parent(zeus, hermes).
parent(maia, hermes).
parent(zeus, athena).
parent(zeus, ares).
parent(hera, ares).
parent(zeus, hephaestus).
parent(hera, hephaestus).
parent(zeus, apollo).
parent(leto, apollo).
parent(zeus, artemis).
parent(leto, artemis).
parent(zeus, dionysius).
parent(semele, dionysius).
parent(aphrodite, harmonia).
parent(ares, harmonia).
parent(harmonia, semele).
parent(demeter, persephone).

female(gaea).
female(rhea).
female(hera).
female(hestia).
female(demeter).
female(athena).
female(metis).
female(maia).
female(persephone).
female(aphrodite).
female(artemis).
female(leto).


male(uranus).
male(cronus).
male(zeus).
male(hades).
male(hermes).
male(apollo).
male(dionysius).
male(hephaestus).
male(poseidon).


