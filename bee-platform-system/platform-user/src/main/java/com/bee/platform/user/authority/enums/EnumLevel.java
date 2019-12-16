package com.bee.platform.user.authority.enums;

/**
 * @ClassName EnumLevel
 * @Description 角色level
 */

public class EnumLevel {
    public enum Level{
        application(1,"应用类角色"),
        function_one(2,"功能类一级角色"),
        function_two(3,"功能类二级角色"),
        basic(4,"基础角色"),
        custom(5,"用户自定义角色"),

        ;

        Level() {
        }

        Level(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        private Integer key;
        private String value;

        public Integer getKey() {
            return key;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }

}
