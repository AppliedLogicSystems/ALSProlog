

/*
 * hickory.pro  -- hickory tree twig description database
 *
 * Copyright (c) 1986, 1988 by Applied Logic Systems
 *
 * Author:  Kenneth A. Bowen
 * Revision History: 
 *      mm/dd/yy,       Name	--	Reason
 *      mm/dd/yy,       Name	--	Reason
 */

/*
 *	Description:
 *
 *	The predicate is trait(<Character>, <Tree>);  the form of
 * 	<Character> is <Generic Character>(<Particular Aspect>).
 *
 */

:-reconsult(id).					% the diagnosis system

module identify.

 trait(bud_scales(valvate),bitternut_hickory).
 trait(buds(yellow),bitternut_hickory).

 trait(bud_scales(valvate),pecan_hickory).
 trait(buds(brownish),pecan_hickory).

 trait(bud_scales(imbricate),pignut_hickory).
 trait(terminal_buds(short),pignut_hickory).

 trait(bud_scales(imbricate), mockernut_hickory).
 trait(terminal_buds(large),mockernut_hickory).
 trait(outer_scales(deciduous),mockernut_hickory).

 trait(bud_scales(imbricate),shellbark_hickory).
 trait(terminal_buds(large),shellbark_hickory).
 trait(outer_scales(persistent),shellbark_hickory).
 trait(twigs(orange_brown),shellbark_hickory).

 trait(bud_scales(imbricate),shagbark_hickory).
 trait(terminal_buds(large),shagbark_hickory).
 trait(outer_scales(persistent),shagbark_hickory).
 trait(twigs(reddish_brown),shagbark_hickory).

endmod.
