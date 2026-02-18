dividerColor := 707070

all: assets/montage.png \
     .aux/montage.txt

.PHONY: all clean


# ---

assets/%.png: .aux/%.png
	optipng -quiet -clobber -out $@ $<


.aux/montage.png: .aux/captured_tidy.png \
                  .aux/captured_checks.png
	magick \
	  -background '#$(dividerColor)' $^ \
	  +smush 1 $@


.aux/montage.txt: .aux/captured_tidy.txt \
                  .aux/captured_checks.txt
	cat $^ >$@


captured_%.png: ; ./scripts/montage/capture       $@
captured_%.txt: ; ./scripts/montage/capture --txt $@


clean:
	rm -f .aux/* 2>/dev/null
