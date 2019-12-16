package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @notes 企业表
 * @Author junyang.li
 * @Date 11:02 2019/3/22
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("enterprises")
public class Enterprises extends Model<Enterprises> {

    private static final long serialVersionUID = 1L;
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业名称
     */
    private String name;
    /**
     * 企业logo字段
     */
    private String head;
    /**
     * 企业管理员
     */
    private String admin;
    /**
     * 公司联系方式
     */
    private String contact;
    /**
     * 企业所属行业
     */
    private String industry;
    /**
     * 指定联系人
     */
    private String linkman;
    /**
     * 详细街道地址
     */
    private String street;
    /**
     * 县级地区id
     */
    private String regionid;
    /**
     * 地址
     */
    private String address;
    /**
     * 数据状态（0删除 1正常）
     */
    private Integer status;
    /**
     * 企业类型 1企业 2物流商
     */
    private Integer type;
    /**
     * 执照号码
     */
    private String licence;
    /**
     * 执照附件
     */
    private String enclosure;
    /**
     * 认证通过时间
     */
    private Date createAt;
    /**
     * 修改时间
     */
    private Date updateAt;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
