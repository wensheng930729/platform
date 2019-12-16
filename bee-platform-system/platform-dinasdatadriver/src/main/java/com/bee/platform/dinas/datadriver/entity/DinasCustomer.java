package com.bee.platform.dinas.datadriver.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 客户表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Data
@ToString
@Accessors(chain = true)
public class DinasCustomer extends Model<DinasCustomer> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 客户名称
     */
    private String customerName;
    /**
     * 类型0采购商 1销售商
     */
    private Integer type;
    /**
     * 状态
     */
    private Integer status;
    /**
     * 删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新人id
     */
    private Integer updateUser;
    /**
     * 更新时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
