package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 仓库单明细
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_repo_receipt_detail")
public class ErpRepoReceiptDetail extends Model<ErpRepoReceiptDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 关联的仓库单据id
     */
    private Integer receiptId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 提单号
     */
    private String voucherNo;
    /**
     * 湿重
     */
    private BigDecimal wetWeight;
    /**
     * 单位
     */
    private String unit;
    /**
     * 仓库id
     */
    private Integer repositoryId;
    /**
     * 化验单id
     */
    private Integer testId;
    /**
     * 料批id
     */
    private Integer batchId;
    /**
     * 水分率
     */
    private String waterRate;
    /**
     * 数量
     */
    private BigDecimal num;
    /**
     * 采购批次号
     */
    private String purchaseBatch;
    /**
     * 车牌号
     */
    private String plateNo;
    /**
     * 毛重
     */
    private BigDecimal roughWeight;
    /**
     * 皮重
     */
    private BigDecimal weight;
    /**
     * 品位
     */
    private BigDecimal grade;
    private Integer createUser;
    /**
     * 记录日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;
    /**
     * 是否删除：1是 0否
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
