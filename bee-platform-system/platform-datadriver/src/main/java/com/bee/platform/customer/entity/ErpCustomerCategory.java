package com.bee.platform.customer.entity;

import java.io.Serializable;
import java.util.Date;

import com.baomidou.mybatisplus.annotations.TableField;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 客户分类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ErpCustomerCategory extends Model<ErpCustomerCategory> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业ID
     */
    private Integer enterpriseId;
    /**
     * 客户二级分类名称
     */
    private String name;
    /**
     * 客户一级分类code
     */
    @TableField("p_code")
    private String pcode;
    /**
     * 客户一级分类名称
     */
    private String pName;
    /**
     * 1-启用，0-禁用
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
