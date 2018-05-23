/* global exports */
"use strict";

// module Elm.Http

exports.encodeUri = function (string) {
    return encodeURIComponent(string);
};

exports.decodeUriImpl = function (Just) {
    return function (Nothing) {
        return function (string) {
            try {
                return Just (decodeURIComponent(string));
            } catch (e) {
                return Nothing;
            }
        }
    }
};
