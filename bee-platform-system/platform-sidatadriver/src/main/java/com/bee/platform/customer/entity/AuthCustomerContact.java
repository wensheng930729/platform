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
 * 客户关联联系人中间表
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-20
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_customer_contact")
public class AuthCustomerContact extends Model<AuthCustomerContact> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 客户ID
     */
    private Integer customerId;
    /**
     * 联系人ID
     */
    private Integer contactId;
    /**
     * 状态 ：1是启用 2是禁用
     */
    private Integer status;
    /**
     * 该客户下角色的序列号
     */
    private Integer orderNum;
    /**
     * 创建人
     */
    private String createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 操作者id
     */
    private Integer operateId;
    /**
     * 表示逻辑删除，1-是删除，0-不删除
     */
    private Integer deleted;
    /**
     * 删除时间
     */
    private Date deletedTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
