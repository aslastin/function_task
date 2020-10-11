#pragma once

#include <type_traits>
#include <exception>

using obj_t = std::aligned_storage<sizeof(void*), alignof(void*)>::type;

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
    using invoker_t = R(*)(obj_t, Args...);
    using deleter_t = void(*)(obj_t);
    using cloner_t = obj_t(*)(obj_t);
    using mover_t = obj_t(*)(obj_t&&);

    invoker_t invoker;
    deleter_t deleter;
    cloner_t cloner;
    mover_t mover;
};

template <typename R, typename... Args>
methods<R, Args...> const* get_empty_methods() {
    static constexpr obj_t empty = obj_t();
    static constexpr methods<R, Args...> table {
        [](obj_t, Args...) -> R {
            throw bad_function_call();
        },
        [](obj_t){},
        [](obj_t){return empty;},
        [](obj_t&&){return empty;}
    };
    return &table;
}

template <typename T, bool IsSmall>
struct object_traits;

template <typename T>
struct object_traits<T, false> {
    template <typename R, typename... Args>
    methods<R, Args...> const* get_methods() {
        static constexpr methods<R, Args...> table {
            [](obj_t obj, Args... args) -> R {
                return (*reinterpret_cast<T*&>(obj))(std::forward<Args>(args)...);
            },
            [](obj_t obj) {
                delete reinterpret_cast<T*&>(obj);
            },
            [](obj_t obj) -> obj_t {
                obj_t fresh_obj;
                reinterpret_cast<T*&>(fresh_obj) = new T(*reinterpret_cast<T*&>(obj));
                return fresh_obj;
            },
            [](obj_t&& obj) -> obj_t {
                obj_t fresh_obj;
                reinterpret_cast<T*&>(fresh_obj) = reinterpret_cast<T*&>(obj);
                reinterpret_cast<T*&>(obj) = nullptr;
                return fresh_obj;
            }
        };
        return &table;
    }
};

template <typename T>
struct object_traits<T, true> {
    template <typename R, typename... Args>
    methods<R, Args...> const* get_methods() {
        static constexpr methods<R, Args...> table {
            [](obj_t obj, Args... args) -> R {
                return reinterpret_cast<T&>(obj)(std::forward<Args>(args)...);
            },
            [](obj_t obj) {
                reinterpret_cast<T*>(&obj)->~T();
            },
            [](obj_t obj) -> obj_t {
                obj_t fresh_obj;
                new (&fresh_obj) T(reinterpret_cast<T&>(obj));
                return fresh_obj;
            },
            [](obj_t&& obj) -> obj_t {
                obj_t fresh_obj;
                new (&fresh_obj) T(std::move(reinterpret_cast<T&>(obj)));
                return fresh_obj;
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

    function(function const& other)
        : obj(other.methods->cloner(other.obj))
        , methods(other.methods)
    {}

    function(function&& other) noexcept
        : obj(other.methods->mover(std::move(other.obj)))
        , methods(other.methods)
    {}

    template <typename T>
    function(T val) {
        if (is_small<T>) {
            new (&obj) T(std::move(val));
        } else {
            reinterpret_cast<void*&>(obj) = new T(val);
        }
        methods = object_traits<T, is_small<T>>().template get_methods<R, Args...>();
    }

    void swap(function& other) noexcept {
        using std::swap;
        std::swap(obj, other.obj);
        std::swap(methods, other.methods);
    }

    function& operator=(function const& rhs) {
        if (this != &rhs) {
            function(rhs).swap(*this);
        }
        return *this;
    }

    function& operator=(function&& rhs) noexcept {
        if (this != &rhs) {
            function(std::move(rhs)).swap(*this);
        }
        return *this;
    }

    ~function() {
        methods->deleter(obj);
    }

    explicit operator bool() const noexcept {
        return methods != get_empty_methods<R, Args...>();
    }

    R operator()(Args... args) const {
        return methods->invoker(obj, std::forward<Args>(args)...);
    }

    template <typename T>
    T* target() noexcept {
        if (methods == object_traits<T, is_small<T>>().template get_methods<R, Args...>()) {
            if (is_small<T>) {
                return reinterpret_cast<T*>(&obj);
            } else {
                return reinterpret_cast<T*&>(obj);
            }
        }
        return nullptr;
    }

    template <typename T>
    T const* target() const noexcept {
        if (methods == object_traits<T, is_small<T>>().template get_methods<R, Args...>()) {
            if (is_small<T>) {
                return reinterpret_cast<const T*>(&obj);
            } else {
                return reinterpret_cast<T* const&>(obj);
            }
        }
        return nullptr;
    }

 private:
    obj_t obj;
    methods<R, Args...> const* methods;
};
