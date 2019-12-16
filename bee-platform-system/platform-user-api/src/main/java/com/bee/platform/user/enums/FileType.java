package com.bee.platform.user.enums;

import lombok.Getter;

/**
 * @description: 文件类型
 * @author: junyang.li
 * @create: 2019-03-22 11:16
 **/
@Getter
public enum  FileType {

    /**
     * 文件类型
     */
    IMAGE("图片"),
    FILE("文件");

    private String desc;

    FileType(){

    }

    FileType(String desc) {
        this.desc = desc;
    }
}
