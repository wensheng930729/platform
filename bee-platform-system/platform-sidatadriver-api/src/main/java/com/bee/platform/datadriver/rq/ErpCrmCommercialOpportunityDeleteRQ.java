package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
/**
 * 
 * @author dell
 *
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
public class ErpCrmCommercialOpportunityDeleteRQ implements Serializable{
	
	private static final long serialVersionUID = -6136098369488265041L;

	/**
	 * 商机客户id
	 */
	private Integer id;
	
	/**
	 * 营销人员id
	 */
	private Integer saleUserId;
	/**
	 * deleted逻辑删除字段，1删除，0未删除
	 */
	private Integer deleted;
	
	
}
