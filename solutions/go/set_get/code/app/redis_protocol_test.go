package main

import (
	"bufio"
	"bytes"
	"testing"
)

func TestDecodeSimpleString(t *testing.T) {
	value, err := DecodeRESP(bufio.NewReader(bytes.NewBufferString("+foo\r\n")))

	if err != nil {
		t.Errorf("error decoding simple string: %s", err)
	}

	if value.typ != SimpleString {
		t.Errorf("expected SimpleString, got %v", value.typ)
	}

	if value.String() != "foo" {
		t.Errorf("expected 'foo', got '%s'", value.String())
	}
}

func TestDecodeBulkString(t *testing.T) {
	value, err := DecodeRESP(bufio.NewReader(bytes.NewBufferString("$4\r\nabcd\r\n")))

	if err != nil {
		t.Errorf("error decoding bulk string: %s", err)
	}

	if value.typ != BulkString {
		t.Errorf("expected BulkString, got %v", value.typ)
	}

	if value.String() != "abcd" {
		t.Errorf("expected 'abcd', got '%s'", value.String())
	}
}

func TestDecodeBulkStringArray(t *testing.T) {
	value, err := DecodeRESP(bufio.NewReader(bytes.NewBufferString("*2\r\n$3\r\nGET\r\n$4\r\nthis\r\n")))

	if err != nil {
		t.Errorf("error decoding array: %s", err)
	}

	if value.typ != Array {
		t.Errorf("expected Array, got %v", value.typ)
	}

	if value.Array()[0].String() != "GET" {
		t.Errorf("expected 'GET', got '%s'", value.Array()[0].String())
	}

	if value.Array()[1].String() != "this" {
		t.Errorf("expected 'this', got '%s'", value.Array()[1].String())
	}
}
