package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售结算单结算详情
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_sale_stmt_detail")
public class ErpSaleStmtDetail extends Model<ErpSaleStmtDetail> {

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
    private Integer product;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 车牌号
     */
    private String plateNo;
    /**
     * 发货数量
     */
    private BigDecimal srcNum;
    /**
     * 收货数量
     */
    private BigDecimal receiveNum;
    /**
     * 结算数量
     */
    private BigDecimal realNum;
    /**
     * 发货品位
     */
    private BigDecimal srcGrade;
    /**
     * 收货品位
     */
    private BigDecimal receiveGrade;
    /**
     * 结算品位
     */
    private BigDecimal realGrade;
    /**
     * 品位误差
     */
    private BigDecimal gradeError;
    /**
     * 发货金额
     */
    private BigDecimal srcAmount;
    /**
     * 结算单价
     */
    private BigDecimal realPrice;
    /**
     * 结算金额
     */
    private BigDecimal realAmount;
    /**
     * 盈亏金额
     */
    private BigDecimal balance;
    private Date createTime;
    private Integer deleted;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    /**
     * 结算扣款
     */
    private BigDecimal realDebit;
    /**
     * 产品批次id
     */
    private Integer productBatchId;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
