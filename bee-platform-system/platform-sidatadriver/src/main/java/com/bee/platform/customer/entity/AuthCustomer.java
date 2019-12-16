package com.bee.platform.customer.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 客户
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-20
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_customer")
public class AuthCustomer extends Model<AuthCustomer> {

    private static final long serialVersionUID = 1L;

    /**
     * 客户表主键ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业ID
     */
    private Integer enterpriseId;
    /**
     * 客户编码
     */
    private String cusNo;
    /**
     * 客户姓名
     */
    private String cusName;
    /**
     * 客户简称
     */
    private String simpleName;
    /**
     * 客户一级分类
     */
    private String cusFirstType;
    /**
     * 客户二级分类
     */
    private String cusSecondType;
    /**
     * 用户状态：1启用 2禁用
     */
    private Integer status;
    /**
     * 表示逻辑删除，1-是删除，0-不删除
     */
    private Integer deleted;
    /**
     * 操作者id
     */
    private Integer operateId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 删除时间
     */
    private Date deletedTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
