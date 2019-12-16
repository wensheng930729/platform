package com.bee.platform.datadriver.entity;

import java.io.Serializable;
import java.math.BigDecimal;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
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
 * 
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
@TableName("erp_repository_receipt")
public class ErpRepositoryReceipt extends Model<ErpRepositoryReceipt> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 单号
     */
    private String code;
    /**
     * 仓库单据类别: 从码表取值
     */
    private String type;
    /**
     * 单据日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptDate;
    /**
     * 关联的源单号
     */
    private String relatedOrder;
    /**
     * 所属公司id
     */
    private Integer orgId;
    /**
     * 关联的源单据id
     */
    private Integer relatedOrderId;
    /**
     * 料批id
     */
    private Integer batchId;
    /**
     * 总数量
     */
    private BigDecimal totalNum;
    /**
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 班次，从码表取值
     */
    private String workShift;
    /**
     * 备注
     */
    private String remark;
    /**
     * 状态，从码表取值
     */
    private Integer state;
    private Integer createUser;
    private Integer updateUser;
    private Date createTime;
    private Date updateTime;
    /**
     * 是否删除：1是 0否
     */
    private Integer deleted;
    /**
     * 附件地址
     */
    private String url;
    /**
     * 创建人企业id
     */
    private Integer enterpriseId;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
