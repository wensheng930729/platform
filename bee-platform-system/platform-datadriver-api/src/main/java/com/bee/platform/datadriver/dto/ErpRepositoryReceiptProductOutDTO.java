package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpRepositoryReceiptRawInDTO
 * @Description 功能描述
 * @Date 2019/6/5 11:36
 **/


@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("成品出库根据id查询详情返回信息")
@JsonInclude
public class ErpRepositoryReceiptProductOutDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 单号
     */
    @ApiModelProperty("单号")
    private String code;

    /**
     * 单据日期
     */
    @ApiModelProperty("单据日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptDate;
    /**
     * 关联的源单号
     */
    @ApiModelProperty("关联的源单号")
    private String relatedOrder;

    /**
     * 关联的源单据id
     */
    @ApiModelProperty("关联的源单据id")
    private Integer relatedOrderId;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String remark;


    @ApiModelProperty("原料入库根据id查询详情入库明细返回信息")
    List<ErpRepoReceiptDetailProductOutDTO> detailProductOutDTOList;



}
