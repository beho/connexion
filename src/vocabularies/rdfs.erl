-module (rdfs).

-compile (export_all).

-include ("rdfs.hrl").

class() -> ?rdfs?Class.
container() -> ?rdfs?Container.
containerMembershipProperty() -> ?rdfs?ContainerMembershipProperty.
datatype() -> ?rdfs?Datatype.
literal() -> ?rdfs?Literal.
resource() -> ?rdfs?Resource.

comment() -> ?rdfs?comment.
domain() -> ?rdfs?domain.
isDefinedBy() -> ?rdfs?isDefinedBy.
label() -> ?rdfs?label.
member() -> ?rdfs?member.
seeAlso() -> ?rdfs?seeAlso.
range() -> ?rdfs?range.
subClassOf() -> ?rdfs?subClassOf.
subPropertyOf() -> ?rdfs?subPropertyOf.

interpretedProperties() ->
	[?rdfs?domain, ?rdfs?range, ?rdfs?subClassOf, ?rdfs?subPropertyOf].
