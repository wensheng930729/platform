package com.bee.platform.datadriver.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 采购结算单结算详情
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_purchase_stmt_detail")
public class ErpPurchaseStmtDetail extends Model<ErpPurchaseStmtDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 结算单id
     */
    private Integer statementId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 结算湿重
     */
    private BigDecimal realWetWeight;
    /**
     * 结算水分
     */
    private BigDecimal realWater;
    /**
     * 结算干重
     */
    private BigDecimal realDryWeight;
    /**
     * 结算扣款
     */
    private BigDecimal realDebit;
    /**
     * 结算单价
     */
    private BigDecimal realPrice;
    /**
     * 结算金额
     */
    private BigDecimal realAmount;
    /**
     * 进厂湿吨
     */
    private BigDecimal srcWetWeight;
    /**
     * 扣水
     */
    private BigDecimal waterDebit;
    /**
     * 进厂干重
     */
    private BigDecimal srcDryWeight;
    /**
     * 干吨盈亏
     */
    private BigDecimal dryBalance;
    /**
     * 实际进厂成本
     */
    private BigDecimal cost;
    /**
     * 发票数量
     */
    private Integer invoiceCount;
    /**
     * 发票金额
     */
    private BigDecimal invoiceAmount;

    private Date createTime;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date statementTime;

    private Integer deleted;
    private BigDecimal grade;

    /**
     * 产品批次id
     */
    private Integer productBatchId;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
