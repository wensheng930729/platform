package com.bee.platform.dinas.datadriver.entity;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 采购付款单
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("dinas_purchase_pay")
public class DinasPurchasePay extends Model<DinasPurchasePay> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 编号
     */
    private String code;
    /**
     * 合同id
     */
    private Integer orderId;
    /**
     * 公司
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 供应商id
     */
    private Integer customerId;
    /**
     * 金额
     */
    private BigDecimal amount;
    /**
     * 付款日期
     */
    private Date payDate;
    /**
     * 附件地址
     */
    private String url;
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
     * 修改时间
     */
    private Date updateTime;
    /**
     * 备注
     */
    private String remark;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
