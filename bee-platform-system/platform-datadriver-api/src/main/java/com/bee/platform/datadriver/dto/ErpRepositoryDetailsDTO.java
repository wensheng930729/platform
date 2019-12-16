package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("仓库档案明细")
public class ErpRepositoryDetailsDTO implements Serializable{
	 /**
	 * 
	 */
	private static final long serialVersionUID = -8660403801235472857L;
	/**
     * 名称
     */
    private String name;
    /**
     * id
     */
    private Integer id;
    /**
     * 状态
     */
    private Integer status;
    /**
     * 所属企业id
     */
    private Integer orgId;
    
    /**
     * 删除状态
     */
    private Integer deleted;

    /**
     * 公司名称
     */
    private String companyName;
}
