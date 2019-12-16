package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品信息表
 * </p>
 *
 * @author qhwang
 * @since 2019-04-26
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("app")
public class App extends Model<App> {

    private static final long serialVersionUID = -6564431551095463343L;

    /**
     * ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 产品logo
     */
    private String logo;
    private String secret;
    /**
     * 产品名称
     */
    private String name;
    /**
     * 产品字母缩写
     */
    private String abbreviation;
    /**
     * 产品链接
     */
    private String uri;
    /**
     * 产品简介
     */
    private String introduction;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 其他信息
     */
    private String remark;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
