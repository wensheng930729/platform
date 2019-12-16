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
 * 采购结算单
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_purchase_statement")
public class ErpPurchaseStatement extends Model<ErpPurchaseStatement> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 结算单号
     */
    private String code;
    /**
     * 订单id
     */
    private Integer orderId;
    /**
     * 采购单公司所属id
     */
    private Integer companyId;
    /**
     * 进厂干重
     */
    private BigDecimal srcDryWeight;
    /**
     * 结算干重
     */
    private BigDecimal realDryWeight;
    /**
     * 干重盈亏
     */
    private BigDecimal dryBalance;
    /**
     * 结算金额
     */
    private BigDecimal amount;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 0未删除 1删除
     */
    private Integer deleted;
    /**
     * 结算日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date statementTime;
    /**
     * 状态: 0已保存 1已确认
     */
    private Integer state;
    /**
     * 附件地址
     */
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
