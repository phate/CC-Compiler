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
	struct                                  { \s -> TStruct }
  typedef                                 { \s -> TTypeDef }
  class                                   { \s -> TClass }
  extends                                 { \s -> TExtends }
  self                                    { \s -> TSelf }
  null                                    { \s -> TNull }
  true																		{ \s -> TTrue }
	false																		{ \s -> TFalse }
  new                                     { \s -> TNew }
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
  \[                                      { \s -> TOSB }
  \]                                      { \s -> TCSB }
	\{																			{ \s -> TOCB }
	\}																			{ \s -> TCCB }
	\;																			{ \s -> TSemi }
	\,																			{ \s -> TComma }
  \.                                      { \s -> TDot }
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
  \-\>                                    { \s -> TDerf }
