dividerColor := 707070

all: assets/montage.png

.PHONY: all clean


# ---

assets/%.png: .aux/%.png
	optipng -quiet -clobber -out $@ $<


.aux/montage.png: .aux/captured_tidy.png \
                  .aux/captured_checks.png
	magick \
	  -background '#$(dividerColor)' $^ \
	  +smush 1 $@


captured_%.png: ; ./scripts/montage/capture       $@


clean:
	rm -f .aux/* 2>/dev/null
