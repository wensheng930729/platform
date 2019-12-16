package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售结算单
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_sale_statement")
public class ErpSaleStatement extends Model<ErpSaleStatement> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 结算单号
     */
    private String code;
    /**
     * 发货数量
     */
    private BigDecimal srcNum;
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
     * 发货金额
     */
    private BigDecimal srcAmount;
    /**
     * 结算金额
     */
    private BigDecimal realAmount;
    private Date createTime;
    private String saleOrder;
    private Integer saleOrderId;
    private Integer state;
    private Integer deleted;
    private Integer companyId;
    private String url;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建人所在企业id
     */
    private Integer enterpriseId;
    /**
     * 更新人
     */
    private Integer updateUser;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
