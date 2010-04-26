{
module Scanner (alexScanTokens) where
import Parser
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$ac = [\0-\255]

tokens :-
	$white+																	;
	"/*"([$ac#\*]|\*[$ac # \/])*("*")+"/"   ;
  "//"[.]*																;
	"#"[.]*																	;
	if																			{ \s -> TIf }
	else																		{ \s -> TElse }
	while																		{ \s -> TWhile }
	return																	{ \s -> TReturn }
	true																		{ \s -> TTrue }
	false																		{ \s -> TFalse }
	int																			{ \s -> TTInt }
	double																	{ \s -> TTDouble }
	boolean																	{ \s -> TTBool }
	void																		{ \s -> TTVoid }
	\".*\"																	{ \s -> TPString s }
	$alpha ($alpha|$digit|\_)*							{ \s -> TId s }
	$digit+\.$digit+((e|E)(\+|\-)?$digit+)?	{ \s -> TPDouble (read s) }
	$digit+																	{ \s -> TPInt (read s) }	
	\(																			{ \s -> TOB }
	\)																			{ \s -> TCB }
	\{																			{ \s -> TOCB }
	\}																			{ \s -> TCCB }
	\;																			{ \s -> TSemi }
	\,																			{ \s -> TComma }
	\+\+																		{ \s -> TIncr }
	\-\-																		{ \s -> TDecr }
	\=																			{ \s -> TAss }
	\|\|																		{ \s -> TOr }
	\&\&																		{ \s -> TAnd }
	\!																			{ \s -> TNot }
	\-																			{ \s -> TMinus }
	\+																			{ \s -> TPlus }
	\*																			{ \s -> TTimes }
	\/																			{ \s -> TDiv }
	\%																			{ \s -> TMod }
	\<																			{ \s -> TLth }
	\<\=																		{ \s -> TLeq }
	\>																			{ \s -> TGth }
	\>\=																		{ \s -> TGeq }
	\=\=																		{ \s -> TEq }
	\!\=																		{ \s -> TNeq }