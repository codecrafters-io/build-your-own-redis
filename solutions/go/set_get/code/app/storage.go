package main

type Storage struct {
	data map[string]string
}

func NewStorage() *Storage {
	return &Storage{
		data: make(map[string]string),
	}
}

func (kv *Storage) Get(key string) string {
	return kv.data[key]
}

func (kv *Storage) Set(key string, value string) {
	kv.data[key] = value
}
