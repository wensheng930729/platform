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
 * @notes 企业审核表
 * @Author junyang.li
 * @Date 10:57 2019/3/22
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("enterprises_check")
public class EnterprisesCheck extends Model<EnterprisesCheck> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 待审核企业的名称
     */
    private String name;
    /**
     * 公司联系方式
     */
    private String contact;
    /**
     * 企业的执照号码
     */
    private String licence;
    /**
     * 企业的执照附件
     */
    private String enclosure;
    /**
     * 企业的地址
     */
    private String address;
    /**
     * 企业的管理员
     */
    private String admin;
    /**
     * 待审核企业的状态(0: 未通过，1: 已通过, 2: 审核中)
     */
    private Integer type;
    /**
     * 审核员id
     */
    private Integer checkId;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人名称
     */
    private String creator;
    /**
     * 创建日期
     */
    private Date createAt;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人名称
     */
    private String modifier;
    /**
     * 更新日期
     */
    private Date updateAt;
    /**
     * 企业在企业表中的真实id
     */
    private Integer realId;
    /**
     * 县级地区id
     */
    private String regionid;
    /**
     * 指定联系人
     */
    private String linkman;
    /**
     * 详细街道地址
     */
    private String street;
    /**
     * 开户许可证
     */
    private String permit;
    /**
     * 企业认证授权书
     */
    private String certificate;
    /**
     * 企业类型 1企业 2物流商
     */
    private Integer enterprisesType;
    /**
     * 企业所属行业
     */
    private String industry;
    /**
     * 审核失败原因
     */
    private String failureReason;

    /**
     * 数据状态（0删除 1正常）
     */
    private Integer status;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
