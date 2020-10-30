#pragma once

#include <type_traits>
#include <exception>

using storage_t = std::aligned_storage<sizeof(void*), alignof(void*)>::type;

template <typename T>
constexpr bool is_small = sizeof(T) <= sizeof(void*) &&
        (alignof(void*) % alignof(T) == 0) && std::is_nothrow_move_constructible<T>();

struct bad_function_call : std::exception {
    char const* what() const noexcept override {
        return "empty function call";
    }
};

template <typename R, typename... Args>
struct methods {
    R(*invoker)(storage_t const*, Args...);
    void(*deleter)(storage_t*) noexcept ;
    void(*cloner)(storage_t*, storage_t const*);
    void(*mover)(storage_t*, storage_t*) noexcept ;
};

template <typename R, typename... Args>
methods<R, Args...> const* get_empty_methods() {
    static constexpr methods<R, Args...> table {
        [](storage_t const*, Args...) -> R {
            throw bad_function_call();
        },
        [](storage_t*) noexcept {},
        [](storage_t*, storage_t const*){},
        [](storage_t*, storage_t*) noexcept {}
    };
    return &table;
}

template <typename T, bool IsSmall>
struct object_traits;

template <typename T>
struct object_traits<T, false> {
    static T const& cast_const(storage_t const* obj) noexcept {
        return *reinterpret_cast<T* const&>(*obj);
    }

    static T*& cast_to_ptr(storage_t* obj) noexcept {
        return reinterpret_cast<T*&>(*obj);
    }

    template <typename R, typename... Args>
    methods<R, Args...> const* get_methods() {
        static constexpr methods<R, Args...> table {
            [](storage_t const* obj, Args... args) -> R {
                return cast_const(obj)(std::forward<Args>(args)...);
            },
            [](storage_t* obj) noexcept {
                delete cast_to_ptr(obj);
            },
            [](storage_t* dst, storage_t const* src) {
                cast_to_ptr(dst) = new T(cast_const(src));
            },
            [](storage_t* dst, storage_t* src) noexcept {
                cast_to_ptr(dst) = cast_to_ptr(src);;
                cast_to_ptr(src) = nullptr;
            }
        };
        return &table;
    }
};

template <typename T>
struct object_traits<T, true> {
    static T const& cast_const(storage_t const* obj) noexcept {
        return reinterpret_cast<T const&>(*obj);
    }

    static T& cast(storage_t* obj) noexcept {
        return reinterpret_cast<T&>(*obj);
    }

    template <typename R, typename... Args>
    methods<R, Args...> const* get_methods() {
        static constexpr methods<R, Args...> table {
            [](storage_t const* obj, Args... args) -> R {
                return cast_const(obj)(std::forward<Args>(args)...);
            },
            [](storage_t* obj) noexcept {
                cast(obj).~T();
            },
            [](storage_t* dst, storage_t const* src) {
                new (dst) T(cast_const(src));
            },
            [](storage_t* dst, storage_t* src) noexcept {
                new (dst) T(std::move(cast(src)));
            }
        };
        return &table;
    }
};

template <typename F>
struct function;

template <typename R, typename... Args>
struct function<R (Args...)> {
    function() noexcept
        : methods(get_empty_methods<R, Args...>())
    {}

    function(function const& other) {
        other.methods->cloner(&storage, &other.storage);
        methods = other.methods;
    }

    function(function&& other) noexcept {
        other.methods->mover(&storage, &other.storage);
        methods = other.methods;
    }

    template <typename T>
    function(T val) {
        if (is_small<T>) {
            new (&storage) T(std::move(val));
        } else {
            reinterpret_cast<void*&>(storage) = new T(std::move(val));
        }
        methods = object_traits<T, is_small<T>>().template get_methods<R, Args...>();
    }

    function& operator=(function const& rhs) {
        if (this != &rhs) {
            storage_t tmp;
            methods->mover(&tmp, &storage);
            methods->deleter(&storage);
            try {
                rhs.methods->cloner(&storage, &rhs.storage);
                methods->deleter(&tmp);
                methods = rhs.methods;
            } catch (...) {
                methods->mover(&storage, &tmp);
                methods->deleter(&tmp);
                throw;
            }
        }
        return *this;
    }

    function& operator=(function&& rhs) noexcept {
        if (this != &rhs) {
            methods->deleter(&storage);
            methods = rhs.methods;
            methods->mover(&storage, &rhs.storage);
        }
        return *this;
    }

    ~function() {
        methods->deleter(&storage);
    }

    explicit operator bool() const noexcept {
        return methods != get_empty_methods<R, Args...>();
    }

    R operator()(Args... args) const {
        return methods->invoker(&storage, std::forward<Args>(args)...);
    }

    template <typename T>
    T* target() noexcept {
        if (methods == object_traits<T, is_small<T>>().template get_methods<R, Args...>()) {
            if (is_small<T>) {
                return reinterpret_cast<T*>(&storage);
            } else {
                return reinterpret_cast<T*&>(storage);
            }
        }
        return nullptr;
    }

    template <typename T>
    T const* target() const noexcept {
        if (methods == object_traits<T, is_small<T>>().template get_methods<R, Args...>()) {
            if (is_small<T>) {
                return reinterpret_cast<const T*>(&storage);
            } else {
                return reinterpret_cast<T* const&>(storage);
            }
        }
        return nullptr;
    }

 private:
    storage_t storage;
    methods<R, Args...> const* methods;
};
