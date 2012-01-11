-module (rdf).

-compile (export_all).

-include ("rdf.hrl").


alt() -> ?rdf?Alt.
bag() -> ?rdf?Bag.
list() -> ?rdf?List.
property() -> ?rdf?Property.
seq() -> ?rdf?Seq.
statement() -> ?rdf?Statement.
xmlLiteral() -> ?rdf?XMLLiteral.

first() -> ?rdf?first.
object() -> ?rdf?object.
predicate() -> ?rdf?predicate.
rest() -> ?rdf?rest.
subject() -> ?rdf?subject.
type() -> ?rdf?type.
value() -> ?rdf?value.


interpretedProperties() ->
	[?rdf?type].