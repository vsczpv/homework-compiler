#!/usr/bin/env bash

RAW=$( \
	./extract_table.sh source.html /dev/stderr 2>&1 >/dev/null \
		| tail -n +2 \
		| head -n -1 \
		| sed  -e 's/\&lt\;/\</g'   -e 's/\&gt\;/\>/g' \
		       -e 's/\-/Error/g'    -e 's/ACCEPT/Accept/g' \
		       -e 's/SHIFT/Shift/g' -e 's/REDUCE/Reduce/g' \
)

WHERE=1
MAX=1
COUNTING=1
for item in $(head -n 1 <<< "$RAW") ; do
	if [[ ${item:0:1} == "<" ]] ; then
		COUNTING=0
	fi
	if [[ ${COUNTING} == "1" ]] ; then
		WHERE=$[$WHERE + 1]
	fi
	MAX=$[MAX + 1]
done

TOTAL=$(tail -n +2 <<< "$RAW" | wc -l)

PARSETABLE=$( \
	awk "{ for (i=2;i<$[$WHERE+1];i++) printf \$i \"\t\"; print \"\" }" \
		<<< $(tail -n +2 <<< "$RAW") \
)

echo "use crate::syn::syntax::*;"
echo "use crate::syn::syntax::ParserStateAction::*;"
echo "/* One extra for EOF */"
echo "pub const PARSER_TOKEN_CT: usize = $[$WHERE-1]usize;";
echo "pub const PARSER_STATE_CT: usize = ${TOTAL}usize;"
echo "#[rustfmt::skip]"
echo "pub const PARSER_TABLE: [[ParserStateAction; PARSER_TOKEN_CT]; PARSER_STATE_CT] = ["
printf "\t[%s],\n" `tr '\t' ',' <<< "$PARSETABLE"`

echo -e "];\n"

NONTERMS=$( \
	awk "{ for (i=$WHERE;i<$MAX;i++) printf \$i \"\t\"; print \"\" }" \
		<<< $(head -n 1 <<< "$RAW") \
)

GOTOTABLE=$( \
	awk "{ for (i=$WHERE+1;i<$MAX+1;i++) { if (\$i!=\"Error\") printf \"Some(\"\$i \")\t\"; else printf \"None\t\"; } print \"\" }" \
		<<< $(tail -n +2 <<< "$RAW") \
)

echo "pub const PARSER_GOTOTABLE_CT: usize = $[$MAX-$WHERE]usize;"
echo "#[rustfmt::skip]"
echo "pub const GOTO_TABLE: [[Option<ParserState>; PARSER_GOTOTABLE_CT]; PARSER_STATE_CT] = [";

printf "\t[%s],\n" `tr '\t' ',' <<< "$GOTOTABLE"`

echo -e "\n];\n\n/*\n * NOTE: If the root non-terminal isn't 0, bugs may ensue.\n *       Also, please check the order is correct. Really.\n *"

INDEX=0
for item in $NONTERMS; do
	echo -n -e " * $item\t= $INDEX\n"
	INDEX=$[INDEX+1]
done

echo " */"

function split_by {
	perl -E 'say for split quotemeta shift, shift' -- "$1" "$2"
}

echo "#[rustfmt::skip]"
echo "pub const PRODUCTIONS: [ProductionAction; PARSER_PRODUCTIONS_CT] = ["

INDEX=0
PRODSCT=0
while read item; do
	IFS="!" read name prods <<< "$item"
	while read rule ; do
		POPCNT=$(sed 's/\s/\n/g' <<< "$rule" | wc -l)
		if [[ ${POPCNT} == "1" && ${rule} == "î" ]] ; then
			printf "\tProductionAction { jumpid: $INDEX, ct_to_pop: 0 },\t"
		else
			printf "\tProductionAction { jumpid: $INDEX, ct_to_pop: $POPCNT },\t"
		fi
		printf "// %s ::= %s\n" "$name" "$rule"
		PRODSCT=$[$PRODSCT+1]
	done <<< $(split_by "|" "$prods")
	INDEX=$[$INDEX + 1]
done <<< $(sed -e 's/::=/!/g' -e 's/\/\/.*//g' -e '/^\s*$/d' -e 's/;//g' -e 's/ \+/ /g' -e 's/\t//g' grammar.txt)

echo "];"
echo "pub const PARSER_PRODUCTIONS_CT: usize = ${PRODSCT}usize;"
