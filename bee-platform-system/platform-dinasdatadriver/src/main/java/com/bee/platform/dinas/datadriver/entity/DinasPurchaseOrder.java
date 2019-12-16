package com.bee.platform.dinas.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;
import com.baomidou.mybatisplus.enums.IdType;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * <p>
 * 采购合同
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Data
@ToString
@Accessors(chain = true)
public class DinasPurchaseOrder extends Model<DinasPurchaseOrder> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 合同编号
     */
    private String code;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 合同日期
     */
    private Date contractDate;
    /**
     * 供应商id
     */
    private Integer customerId;
    /**
     * 合同附件
     */
    private String url;
    /**
     * 付款金额
     */
    private BigDecimal payment;
    /**
     * 可用金额
     */
    private BigDecimal availableAmount;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人
     */
    private Integer updateUser;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 版本号
     */
    @Version
    private Integer version;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
