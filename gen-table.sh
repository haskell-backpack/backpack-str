#!/bin/sh
sigc str-sig/Str.hsig \
    S=str-string/Str/String.hs \
    T=str-text/Str/Text.hs \
    TL=str-text/Str/Text/Lazy.hs \
    B=str-bytestring/Str/ByteString.hs \
    BC=str-bytestring/Str/ByteString/Char8.hs \
    BL=str-bytestring/Str/ByteString/Lazy.hs \
    BLC=str-bytestring/Str/ByteString/Lazy/Char8.hs
